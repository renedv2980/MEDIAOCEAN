*          DATA SET DEDEFINE   AT LEVEL 104 AS OF 01/11/21                      
*PROCESS USING(WARN(15))           ENABLE ALL USING STATEMENT WARNINGS          
*PHASE T00A26A                                                                  
*INCLUDE MININAM                                                                
DEFINE   TITLE '-   DEMO RECORD DATA EXTRACTOR'                                 
DEFINE   RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 FINEX-FINED,*DFINE,R8,RR=RE                                      
         USING FINED,RC                                                         
         ST    RB,MYBASE1                                                       
         ST    R8,MYBASE2                                                       
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
         LR    R9,R1                                                            
*                                                                               
         LA    R0,GOSUB                                                         
         ST    R0,AGOSUB                                                        
*                                                                               
         L     RA,4(R9)                                                         
         USING DBLOCK,RA                                                        
         MVC   MYDBFILE,DBFILE    <-- USE 'MYDBFILE' THROUGHOUT                 
         MVC   DATADISP,DBDTADSP                                                
         CLC   MYDBFILE,=C'IUN'                                                 
         BE    *+10                                                             
         CLC   MYDBFILE,=C'INV'                                                 
         BNE   *+10                                                             
         MVC   DATADISP,=H'34'                                                  
         L     R6,DBAREC                                                        
         L     R7,DBAQUART                                                      
*                                                                               
         MVI   SPNTFLG,0                                                        
         CLI   DBSELMED,C'N'       NHT FROM SPOT-> FORCE DBFILE=NTI             
         BNE   DEFINE1                                                          
         CLC   =C'TP',MYDBFILE                                                  
         BE    *+10                                                             
         CLC   =C'PAV',MYDBFILE                                                 
         BNE   DEFINE1                                                          
         CLI   DBSELSRC,C'H'       NHT FILE                                     
         BE    *+8                                                              
         CLI   DBSELSRC,C'D'       NAD                                          
         BE    *+8                                                              
         CLI   DBSELSRC,C'C'       NTI CABLE                                    
         BE    *+8                                                              
         CLI   DBSELSRC,C'K'       NTI                                          
         BNE   DEFINE1                                                          
         MVC   MYDBFILE,=C'NAD'    OVERIDE THE GIVEN DBFILE                     
         MVI   SPNTFLG,X'01'       NET LKUP FROM SPOT                           
*                                                                               
DEFINE1  MVI   CBLNAD,0                                                         
         USING PMKEY,R6                                                         
         CLI   PMCODE,PMCODEQU     C'Q' PROGRAM DATA                            
         BNE   DEFINE2                                                          
         CLI   PMMEDIA,C'C'        CABLE                                        
         BE    *+12                                                             
         CLI   PMMEDIA,C'c'        CABLE COMMERCIAL AVERAGE                     
         BNE   DEFINE2                                                          
         DROP  R6                                                               
         LR    RE,R7               DISPLACE OFF OF DBAQUART                     
         SR    RE,R6                                                            
         STH   RE,DATADISP                                                      
*FOR CABLE NAD, DESCRIPTIVE ELEMENTS ARE IN DBSPANAD                            
         ICM   RE,15,DBSPANAD                                                   
         BZ    DEFINE2                                                          
         AHI   RE,320              DISPLACEMENT TO CBL NAD INFO LABEL           
         CLC   0(4,RE),=C'NDEF'                                                 
         BNE   DEFINE2                                                          
         MVI   CBLNAD,1                                                         
         LA    RE,4(RE)            POINT TO CABLE NAD INFO                      
         LR    R6,RE               (KEY + DESCRIPTIVE ELEMENTS)                 
         MVC   DATADISP,DBDTADSP                                                
*                                                                               
DEFINE2  L     R3,8(R9)                                                         
         L     R2,0(R9)                                                         
*                                                                               
*SPECIAL KEYWORDS.                                                              
         CLC   0(5,R2),=C'DPROG'                                                
         BE    DPROGRAM                                                         
*                                                                               
         LARL  RE,KEYDTAB                                                       
         USING KEYDTABD,RE                                                      
DEF1A    CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                KEYWORD NOT FOUND                            
*                                                                               
         LHI   R1,L'KEYWORD                                                     
         LA    RF,L'KEYWORD-1(RE)                                               
DEF2A    CLI   0(RF),C' '          DETERMINE LENGTH OF KEYWORD                  
         BNE   DEF3A                                                            
         BCTR  R1,0                                                             
         BCT   RF,DEF2A                                                         
*                                                                               
DEF3A    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEYWORD(0),0(R2)                                                 
         BE    DEF4A                                                            
         LA    RE,KEYDTABL(RE)                                                  
         B     DEF1A                                                            
*                                                                               
DEF4A    ICM   RF,15,KEYROUT                                                    
         A     RF,RELO                                                          
         TM    KEYFLAG,REL         TEST RELOCATED ROUTINE                       
         BNOR  RF                  GO TO ROUTINE                                
         BASR  RE,RF               EXECUTE ROUTINE                              
         B     XIT                 AND EXIT                                     
         DROP  RE                                                               
*                                                                               
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              PROGRAMS                                                         
         SPACE 1                                                                
PROG     LA    RF,5                                                             
         CLI   SPNTFLG,X'01'       NET FROM SPOT-> USE 16 CHAR NAME             
         BNE   *+12                                                             
         LA    RF,15                                                            
         B     PROG1                                                            
         CLC   0(6,R2),=C'PROG25'                                               
         BNE   *+12                                                             
         LA    RF,24               OPTION FOR 25 FOR SOME FILES                 
         B     PROG1                                                            
         CLC   0(6,R2),=C'PROGRA'                                               
         BNE   *+8                                                              
         LA    RF,15               OPTION FOR 15 FOR TP                         
PROG1    ST    RF,DUB              SAVE LENGTH VALUE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=CL25' '                                                 
*                                                                               
         CLC   MYDBFILE,=C'RLD'    RLD CABLE JUST MOVES PROGRAM NAME            
         BNE   PRGRLDCX                                                         
         L     RE,DBAREC                                                        
         USING MXMKEY,RE                                                        
         CLI   MXMMEDIA,C'C'                                                    
         BNE   PRGRLDCX                                                         
         DROP  RE                                                               
         MVI   ELCODE,X'21'        PROGRAM ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PPNELEM,R6                                                       
         ZIC   RE,PPNELN                                                        
         SHI   RE,PPNNME-PPNELEM                                                
         BCTR  RE,0                                                             
         L     RF,DUB              RESTORE LENGTH REQUESTED                     
         CR    RF,RE               IF GREATER THAN PROG NAME LENGTH             
         BNH   *+6                                                              
         LR    RF,RE               USE THE SMALLER LENGTH                       
         EX    RF,*+8                                                           
         B     XIT                                                              
         OC    0(0,R3),PPNNME      JUST MOVE TO OUTPUT                          
         DROP  R6                                                               
PRGRLDCX DS    0X                                                               
*                                                                               
*        HANDLE SPANNED PROGRAM NAMES                                           
         ICM   RE,15,DBSPANAD      ONLY CABLE IS SPANNED NOW                    
         BZ    PROG1A                                                           
         USING SPANADD,RE                                                       
         CLC   SPDID,=C'CB2'        CABLE NAD AND REG CABLE                     
         BE    *+10                                                             
         CLC   SPDID,=C'CAB'        THIS IDENTIFIES THE SPANNER                 
         BNE   PROG1A                                                           
         EX    RF,*+8                                                           
         B     XIT                                                              
         OC    0(0,R3),SPDPRNAM     JUST MOVE TO OUTPUT FOR NOW                 
         DROP  RE                                                               
*                                                                               
PROG1A   LA    R0,1(RF)                                                         
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    PROG2                                                            
         CLC   MYDBFILE,=C'RLD'                                                 
         BE    PROG2                                                            
         CLC   MYDBFILE,=C'PAV'                                                 
         BE    PROG2                                                            
         CLC   MYDBFILE,=C'MPA'                                                 
         BE    PROG2                                                            
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    PROG2                                                            
         CLC   MYDBFILE,=C'NAD'                                                 
         BE    PROG2                                                            
         CLC   MYDBFILE,=C'INV'                                                 
         BE    PROG7                                                            
         CLC   =C'TP',MYDBFILE                                                  
         BE    PROG8                                                            
         CLC   MYDBFILE,=C'RDP'    RADIO DAYPART FILE                           
         BE    PROG8                                                            
         CLC   MYDBFILE,=C'RTP'    RADAR DAYPART FILE                           
         BE    PROG8                                                            
         CLC   MYDBFILE,=C'CTP'                                                 
         BE    PROG8                                                            
         CLC   MYDBFILE,=C'IUN'    REP INVENTORY FILE                           
         BE    PROG30                                                           
         CLC   MYDBFILE,=C'RUA'    COUNTY COVERAGE                              
         BE    PROG8                                                            
         B     XIT                                                              
         SPACE 1                                                                
PROG2    MVI   ELCODE,X'21'        PAVFILE                                      
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PPNELEM,R6                                                       
         ST    R6,DMCB                                                          
         ZIC   R1,PPNELN                                                        
         SHI   R1,3                                                             
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    PROG3                                                            
         CLC   MYDBFILE,=C'RLD'                                                 
         BE    PROG3                                                            
         CLC   MYDBFILE,=C'NAD'    TEST FOR NAD                                 
         BE    PROG3                                                            
         CLC   0(6,R2),=C'PROG06'                                               
         BNE   *+12                                                             
         CR    R1,R0               DON'T CLOBBER USER'S STORAGE                 
         BL    *+6                                                              
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),PPNNME                                                   
         SPACE 1                                                                
PROG3    L     R6,DBAREC           TEST FOR REDUCED STATION FAC.                
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   PROG3A                                                           
         USING PHTELEM,R6                                                       
         TM    PHTRSF,X'01'                                                     
         BZ    PROG3A                                                           
         SHI   R0,3                ALLOW ROOM FOR (B)                           
         DROP  R6                                                               
         SPACE 1                                                                
PROG3A   L     R6,DMCB             RESTORE PROG. NAME ELEMENT                   
         USING PPNELEM,R6                                                       
         MVC   WORK,PPNNME                                                      
         BRAS  RE,ITNPRGOV         OVERRIDE SPECIAL ITN PROGRAM NAMES           
         CR    R1,R0               TEST FOR LONG PROGRAM NAMES                  
         BH    PROG4                                                            
         EX    R1,*+8                                                           
         B     PROG5                                                            
         MVC   0(0,R3),WORK                                                     
PROG4    LA    RF,1(R1)            RESTORE PROGRAM NAME LENGTH                  
         GOTO1 =V(MININAM),DMCB,((RF),WORK),((R0),(R3)),=C'PNABBR',    X        
               RR=RELO                                                          
PROG5    L     R6,DBAREC                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PHTELEM,R6                                                       
         TM    PHTRSF,X'01'        TEST FOR REDUCED STAT FACILITIES             
         BZ    XIT                                                              
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         CLC   0(16,R3),=CL16' '                                                
         BE    PROG6                                                            
         LA    RE,12(R3)                                                        
         CLC   0(6,R2),=C'PROG06'                                               
         BNE   *+8                                                              
         LA    RE,2(R3)                                                         
         CLC   0(6,R2),=C'PROG25'                                               
         BNE   *+8                                                              
         LA    RE,21(R3)                                                        
         CLC   0(3,RE),=CL3' '                                                  
         BNE   PROG6                                                            
         BCTR  RE,0                                                             
         B     *-12                                                             
PROG6    MVC   1(3,RE),=C'(B)'     MARK THE PROGRAM NAME                        
         B     XIT                                                              
         EJECT                                                                  
PROG7    MVI   ELCODE,X'01'        INV                                          
         BAS   RE,GETEL                                                         
         USING RINVPEL,R6                                                       
         BNE   XIT                                                              
         ZIC   R1,RINVPLEN                                                      
         SHI   R1,41                                                            
         BNP   XIT                                                              
         CHI   R1,15                                                            
         BL    *+8                                                              
         LA    R1,15                                                            
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R3),RINVPROG                                                 
         SPACE 1                                                                
PROG8    BAS   RE,TESTTPN          TP, DPT, & TPT                               
         BNE   PROG9                                                            
         MVI   ELCODE,X'21'                                                     
         B     PROG9A                                                           
PROG9    MVI   ELCODE,X'20'        TP, DPT, & TPT                               
PROG9A   BAS   RE,GETEL                                                         
         B     PROG12                                                           
         SPACE 1                                                                
PROG10   BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
PROG12   BNE   XIT                                                              
*                                                                               
         BAS   RE,TESTTPN          TP AND NSI?                                  
         BNE   PROG12H                                                          
*                                                                               
         USING QIELEM,R6                                                        
         LR    RE,R6               SAVE IT JUST IN CASE                         
*                                                                               
         CLI   QIELN,4             DO WE HAVE A DUP?  4 BYTES LONG              
         BNE   PROG12C             NO, USE THE NAME                             
         NI    QIPNUM,X'7F'                                                     
         SR    R1,R1                                                            
         ICM   R1,3,QIPNUM         GRAB THE DISPLACEMENT FROM REC               
         L     RF,DBAREC                                                        
         AR    RF,R1                                                            
         LR    R6,RF                                                            
PROG12C  MVC   0(6,R3),QIPNAM6                                                  
         MVC   WORK(6),0(R3)                                                    
         LR    R6,RE               RESTORE THE CURRENT ELEMENT                  
         B     PROG12X                                                          
*                                                                               
         USING QHELEM,R6                                                        
PROG12H  ZIC   R1,QHELN            DIG OUT LAST NAME                            
         SHI   R1,7                                                             
         BM    PROG13                                                           
         MVC   0(16,R3),=CL16' '                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),QHPNAME **EXECUTED**                                     
         MVC   WORK(16),0(R3)      SAVE FULL NAME                               
*                                                                               
PROG12X  L     RF,DBAREC           CONVERSION PUTS A NUMBER                     
         CLC   =C'NOT GIVEN',WORK                                               
         BNE   *+10                                                             
         MVC   WORK(9),=C'VARIOUS  '                                            
         CLC   0(3,RF),=C'RCA'     AT THE END OF PROGRAM NAME                   
         BNE   *+16                TO ELIMINATE DUPS                            
         TM    13(R3),X'F0'        ZAP IT HERE                                  
         BNO   *+8                                                              
         MVI   13(R3),C' '                                                      
         EJECT                                                                  
* TEST REQUEST INCLUDES NUMBER OF WEEKS *                                       
         SPACE 1                                                                
PROG13   CLC   =C'PROG+',0(R2)                                                  
         BNE   PROG20                                                           
         SPACE 1                                                                
* COUNT NUMBER OF WEEKS *                                                       
         SPACE 1                                                                
PROG14   CLI   ELCODE,X'20'        TEST TP                                      
         BNE   PROG20                                                           
         CLI   DBSELMED,C'U'       RADIO COUNTY COVERAGE DONT HAVE              
         BE    PROG20              WEEKS                                        
         MVC   0(16,R3),WORK       RESTORE AND DO NEW WEEKS                     
         TM    QHWKS,X'0F'         TEST 4 WEEKS                                 
         BO    PROG20              YES                                          
*                                                                               
         ZIC   RE,QHWKS                                                         
         SLL   RE,28                                                            
         SR    RF,RF                                                            
*                                                                               
PROG16   LTR   RE,RE                                                            
         BZ    PROG16X                                                          
         BP    *+6                                                              
         BCTR  RF,0                ADD TO COUNTER                               
         SLL   RE,1                                                             
         B     PROG16                                                           
*                                                                               
PROG16X  LPR   RF,RF                                                            
         LA    RF,240(RF)          MAKE COUNT EBCDIC                            
         MVC   12(3,R3),=C'(X)'                                                 
         STC   RF,13(R3)                                                        
         SPACE 1                                                                
PROG20   CR    R6,R7               HAVE WE REACHED THIS 1/4 HOUR                
         BL    PROG10                                                           
         B     XIT                                                              
*                                                                               
PROG30   ICM   RE,15,DBEXTEND                                                   
         BZ    XIT                                                              
PROG32   CLC   0(4,RE),=C'RINV'                                                 
         BE    PROG34                                                           
         ICM   RE,15,4(RE)                                                      
         BZ    XIT                                                              
         B     PROG32                                                           
         USING DBXINVWK,RE                                                      
PROG34   MVC   0(16,R3),DBXIPROG                                                
         CLC   0(6,R2),=C'PROG25'                                               
         BNE   XIT                                                              
         MVC   0(25,R3),DBXIPROG                                                
         B     XIT                                                              
         DROP  RE                                                               
*                                                                               
TESTTPN  NTR1                                                                   
         CLC   =C'PROG06',0(R2)                                                 
         BNE   NOTTPN                                                           
         CLC   MYDBFILE,=C'TP '                                                 
         BNE   NOTTPN                                                           
         CLI   DBSELSRC,C'N'                                                    
         BNE   NOTTPN                                                           
ISTPN    SR    RE,RE                                                            
NOTTPN   LTR   RE,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
*        CABLE PROGRAMS                                                         
         SPACE 1                                                                
CPROG    LA    RF,5                DEFAULT OUTPUT LEN = 6, NSI TP               
         CLC   0(6,R2),=C'CPROG25'                                              
         BNE   *+12                                                             
         LA    RF,24               OPTION FOR 25 FOR SOME FILES                 
         B     CPROG1                                                           
         CLC   0(6,R2),=C'CPROGRA'                                              
         BNE   *+8                                                              
         LA    RF,15               OPTION FOR 15 FOR TP                         
*                                                                               
CPROG1   ST    RF,DUB              SAVE LENGTH                                  
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=CL25' '                                                 
*                                                                               
         CLC   MYDBFILE,=C'RLD'    MINUTE BY MINUTE DATA HAS PROGRAM            
         BNE   CPROG05             AND TRACK NAMES IN ELEMENTS                  
         XC    WORKL(4+50+4+50),WORKL                                           
         MVI   ELCODE,PPNCODEQ     '21' PROGRAM NAME ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   CPROG02                                                          
         USING PPNELEM,R6                                                       
         ZIC   R1,PPNELN                                                        
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORKL+4(0),PPNNME   PROGRAM NAME                                 
         DROP  R6                                                               
CPROG02  MVI   ELCODE,NTRKCDEQ     '28' TRACKAGE ELEMENT                        
         L     R6,DBAREC                                                        
         BRAS  RE,GETEL                                                         
         BNE   CPROG03                                                          
         USING NTRKELEM,R6                                                      
         ZIC   R1,NTRKLN                                                        
         SHI   R1,5                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORKL+4+50+4(0),NTRKNAME   TRACK NAME                            
         B     CPROG03A                                                         
         DROP  R6                                                               
CPROG03  MVC   WORKL+4+50+4(50),WORKL+4   USE PROG NAME IF NO TRACK             
CPROG03A LA    R1,WORKL            4(R1)=PRG NAME                               
         LA    RE,WORKL+54         4(RE)=TRK NAME                               
         L     RF,DUB              RESTORE LENGTH IN RF                         
         B     CPROG12                                                          
*                                                                               
* NTI HAS PROGRAM AND TRACK NAMES IN DBSPANAD                                   
CPROG05  ICM   RE,15,DBSPANAD      ONLY CABLE IS SPANNED NOW                    
         BZ    PROG1A                                                           
         USING SPANADD,RE                                                       
         CLC   SPDID,=C'CB2'       CABLE NAD AND REG CABLE                      
         BE    *+10                                                             
         CLC   SPDID,=C'CAB'       THIS IDENTIFIES THE SPANNER                  
         BNE   PROG1A                                                           
         ZIC   R0,SPDLEVEL         INDEX TO PROPER PROG NAME                    
         CLI   SPDLEVEL,1          DON'T INCLUDE EPISODE IN CPROG               
         BNH   *+8                                                              
         LA    R0,1                                                             
         MHI   R0,30                                                            
         CLI   SPDLEVEL,1          TRACK LEVEL?                                 
         BNL   CPROG10                                                          
         AR    RE,R0                                                            
         EX    RF,*+8                                                           
         B     XIT                                                              
         OC    0(0,R3),4(RE)       JUST MOVE TO OUTPUT FOR NOW                  
*                                                                               
CPROG10  LR    R1,RE               R1=PRG NAME IN SPANNER                       
         AR    RE,R0               RE=TRK NAME IN SPANNER                       
CPROG12  CLC   4(4,R1),4(RE)       MATCH ON 1ST 4CHAR PRG-TRK NAME              
         BE    CPROG15             COMBINE PRG/TRK NAMES                        
         OC    4(25,RE),4(RE)      MAKE SURE TRK NOT EMPTY                      
         BZ    CPROG15                                                          
         OC    4(25,RE),=CL25' '   MAKE SURE TRK NOT BLANK                      
         BNE   PRGTRK                                                           
*                                                                               
CPROG15  EX    RF,*+8              SAME PRG-TRK NAME                            
         B     XIT                                                              
         OC    0(0,R3),4(RE)       MOVE TRK NAME TO OUTPUT                      
         DROP  RE                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
*PRGTRK- COMBINE PRG NAME AND TRK NAME ON SAME LINE                             
**********************************************************************          
PRGTRK   DS    0H                                                               
*                                                                               
         CLC   MYDBFILE,=C'RLD'    MINUTE BY MINUTE DATA HAS R1 AND RE          
         BE    PRGTR1              ALREADY SET TO WORKL AND WORKL+54            
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,15,DBSPANAD      4(R1)=PRG NAME                               
         LA    RE,30(R1)           4(RE)=TRK NAME                               
*                                                                               
PRGTR1   MVI   0(R3),C' '          40 CHAR BLANK PAD                            
         MVC   1(39,R3),0(R3)       OUTPUT LINE                                 
*                                                                               
         LA    R0,25               CALC L'TRACK NAME W/OUT TRAIL SPCS           
         LA    R5,24+4(RE)         PT TO LAST CHAR IN TRK NAME                  
PRGTR5   CLI   0(R5),0             DELETE TRAILING BLANKS                       
         BE    *+12                                                             
         CLI   0(R5),C' '          DELETE TRAILING BLANKS                       
         BNE   *+10                NON-BLANK--DONE, R0=TRK LENGTH               
         BCTR  R5,0                                                             
         BCT   R0,PRGTR5           R0=L'TRK NAME                                
*                                                                               
         LA    RF,25               CALC L'PRG NAME W/OUT TRAIL SPCS             
         LA    R5,24+4(R1)         PT TO LAST CHAR IN PRG NAME                  
PRGTR8   CLI   0(R5),0             DELETE TRAILING BLANKS                       
         BE    *+12                                                             
         CLI   0(R5),C' '          DELETE TRAILING BLANKS                       
         BNE   *+10                NON-BLANK--DONE, R0=TRK LENGTH               
         BCTR  R5,0                                                             
         BCT   RF,PRGTR8           RF=L'PRG NAME                                
*                                                                               
         LR    R5,RF               L'PRG NAME                                   
         AR    R5,R0               L'PRG NAME + L'TRK NAME                      
         CHI   R5,39               MAX OUTPUT LEGTH (-1 FOR '/')                
         BH    PRGTR20                                                          
         BCTR  RF,0                -1 FOR EXMVC                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,R3),4(R1)       MOVE IN PRG NAME                             
         AR    RF,R3               BUMP OUTPUT ADDRESS                          
         MVI   1(RF),C'/'          SEPERATOR FOR PRG/TRKG NAMES                 
         LA    RF,2(RF)            FOR '/' AND RF'S -1 FOR  EX LENGTH           
         BCTR  R0,0                L'TRK -1                                     
         LR    R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),4(RE)       MOVE IN TRK NAME                             
         B     XIT                                                              
*                                                                               
PRGTR20  DS    0H                  TRUNCATE PRG NAME                            
         LA    R5,40               L'OUTPUT FIELD                               
         SR    R5,R0                 -L'TRK NAME                                
         AR    R5,R3                                                            
         LR    R1,R0               L'TRK NAME                                   
         BCTR  R1,0                FOR EXMVC                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R5),4(RE)       MOVE IN FULL TRK NAME                        
         BCTR  R5,0                                                             
         MVI   0(R5),C'/'                                                       
         SR    R5,R3               REMAINING # SPACES FOR PRG NAME              
         BCTR  R5,0                -1 FOR EX                                    
         ICM   R1,15,DBSPANAD      RESTORE R1->PRG NAME                         
         CLC   MYDBFILE,=C'RLD'    RLD HAS PROGRAM NAME IN WORKL                
         BNE   *+8                                                              
         LA    R1,WORKL                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    0(0,R3),4(R1)       MOVE IN TRUNCATED PRG NAME                   
         B     XIT                                                              
         EJECT                                                                  
*              TRACK TITLES                                                     
         SPACE 3                                                                
TRAK     DS    0H                                                               
         BRAS  RE,TRAKNTR                                                       
         B     XIT                                                              
*              DAILY PROGRAMS                                                   
DPROGRAM MVC   0(6,R2),=C'EPIS25'                                               
         LA    RE,DPROG1           BUILD FAKE D CHAIN ELEMENT                   
         STM   RE,RC,12(RD)                                                     
         ST    RD,76(RD)                                                        
         MVC   0(4,RD),=C'DPRO'                                                 
         LA    RE,72(RD)                                                        
         ST    RE,8(RD)                                                         
         LR    RD,RE                                                            
         LARL  RE,EPIS                                                          
         BASR  RE,RF                                                            
         B     XIT                                                              
DPROG1   CLC   =CL25' ',0(R3)                                                   
         BNE   DPROG2                                                           
         MVC   0(6,R2),=C'PROG25'                                               
         LA    RE,DPROG2           BUILD FAKE D CHAIN ELEMENT                   
         STM   RE,RC,12(RD)                                                     
         ST    RD,76(RD)                                                        
         MVC   0(4,RD),=C'DPRO'                                                 
         LA    RE,72(RD)                                                        
         ST    RE,8(RD)                                                         
         LR    RD,RE                                                            
         B     PROG                                                             
DPROG2   MVC   0(6,R2),=C'DPROG '                                               
         B     XIT                                                              
         EJECT                                                                  
*              TIME EXPRESSIONS                                                 
         SPACE 3                                                                
TIME     XC    0(7,R3),0(R3)                                                    
         CLC   MYDBFILE,=C'NAD'                                                 
         BE    TIME1                                                            
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    TIME1                                                            
         CLC   MYDBFILE,=C'RLD'                                                 
         BE    TIME1                                                            
         CLC   MYDBFILE,=C'PAV'                                                 
         BE    TIME2                                                            
         CLC   MYDBFILE,=C'MPA'                                                 
         BE    TIME2                                                            
         CLC   MYDBFILE,=C'INV'                                                 
         BE    TIME4                                                            
         CLC   MYDBFILE,=C'IUN'                                                 
         BE    TIMIUN                                                           
         CLC   =C'TP',MYDBFILE                                                  
         BE    TIME6                                                            
         CLC   =C'CTP',MYDBFILE                                                 
         BE    TIME6                                                            
         CLC   =C'DPT',MYDBFILE                                                 
         BE    TIME6                                                            
         CLC   MYDBFILE,=C'EVN'                                                 
         BE    TIME6                                                            
         CLC   MYDBFILE,=C'RDP'                                                 
         BE    TIME8                                                            
         B     XIT                                                              
         SPACE 1                                                                
TIMIUN   ICM   R7,15,DBEXTEND                                                   
         BZ    XIT                                                              
TIMIUN2  CLC   0(4,R7),=C'RINV'                                                 
         BE    TIMIUN4                                                          
         ICM   R7,15,4(R7)                                                      
         BZ    XIT                                                              
         B     TIMIUN2                                                          
         USING DBXINVWK,R7                                                      
TIMIUN4  MVC   2(4,R3),DBXISTIM                                                 
                                                                                
         MVC   MTIME,DBXISTIM                                                   
         BAS   RE,MILTOQHR         CONVERT START TO QH (R0=QH ON RETRN)         
         LR    R4,R0               R4 = START QUARTER HOUR                      
         STC   R4,0(R3)                                                         
         MVC   MTIME,DBXIETIM                                                   
         LR    R5,R4               INITIALIZE END QH TO START QH                
         OC    DBXIETIM,DBXIETIM   IF BREAK TIME (I.E. NO END TIME)             
         BZ    TIMIUN5              LEAVE  END QH = START QH                    
         BAS   RE,MILTOQHR         CONVERT END   TO QH (R0=QH ON RETRN)         
         LR    R5,R0                                                            
         SR    R0,R0                                                            
         L     R1,MINUTE                                                        
         D     R0,=F'15'                                                        
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         BCTR  R5,0                                                             
TIMIUN5  EQU   *                                                                
         STC   R5,1(R3)            END QUARTER HOUR                             
         SR    R5,R4                                                            
         LA    R5,1(R5)            R5 = NUMBER OF QUARTER HOURS                 
                                                                                
*                                  TAKE # OF DAYS INTO ACCOUNT                  
         SR    R0,R0                R0 COUNTS # OF DAYS                         
         ZICM  RF,DBXIDAY,(8)                                                   
         SLL   RF,1                HIGH-ORDER-BIT NOT USED IN DAY CODE          
TIMIUN8  LTR   RF,RF                                                            
         BZ    TIMIUN8X                                                         
         SR    RE,RE                                                            
         SLDL  RE,1                                                             
         AR    R0,RE                                                            
         B     TIMIUN8                                                          
TIMIUN8X EQU   *                                                                
         SR    R4,R4                                                            
         MR    R4,R0               TOTAL DURATION = (# QH) * (# DAYS)           
         STC   R5,6(R3)                                                         
         B     XIT                                                              
         DROP  R7                                                               
         SPACE 1                                                                
TIME1    CLI   DBRECTYP,DBRECDEM   TEST FOR 'P' RECORD                          
         BE    TIME2                                                            
         CLI   DBRECTYP,DBRECNTI   TEST FOR 'Q' (NETWK PROG) REC                
         BNE   XIT                                                              
         MVI   ELCODE,X'22'        RUN TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         USING NTELEM,R6                                                        
         MVC   0(1,R3),NTSQH                                                    
         ZIC   R1,NTEQH            BUMP EQH SO IT AGREES W PAV EQH              
         LA    R1,1(R1)                                                         
         STC   R1,1(R3)                                                         
         ZIC   R0,NTSQH            GET SQH                                      
         SR    R1,R0               FIND QHR DURATION                            
         STC   R1,6(R3)                                                         
         MVC   2(4,R3),NTSTIM                                                   
*                                                                               
         CLC   MYDBFILE,=C'RLD'    FOR RLD,                                     
         BNE   TIME1A              ADJUST END TIME                              
         CLC   =C'TIMEEX',0(R2)    EXACT END TIME                               
         BE    TIME1A                                                           
         SR    R1,R1               EXAMPLE: 5-559 DISPLAYS AS 5-6               
         ICM   R1,3,NTETIM         END TIME BEFORE ADJUSTMENT                   
         SR    R0,R0                                                            
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
         AHI   R0,1                ADD 1 MINUTE                                 
         CHI   R0,60               TEST IF >= 1 HOUR                            
         BL    *+12                NO                                           
         SHI   R0,60               SUBTRACT 60 MINUTES                          
         LA    R1,1(R1)            AND ADD 1 TO HOURS                           
         MHI   R1,100              HOURS X 100 FOR MILITARY TIME                
         AR    R1,R0               ADD MINUTES TO HOURS                         
         STCM  R1,3,4(R3)                                                       
*                                                                               
TIME1A   CLC   =C'TIMERD',0(R2)                                                 
         BE    TIMERD              LOOKING FOR ROUNDED TIMES                    
         B     XIT                                                              
         SPACE 1                                                                
         USING PRKEY,R6                                                         
TIME2    MVC   0(1,R3),PRSTIM      PAV                                          
         MVI   ELCODE,X'20'        USE PAV DAY/QTR HR ELEMENT                   
         BAS   RE,GETEL            TO FIND DURATION                             
         USING PHELEM,R6                                                        
         ZIC   R1,PHDUR                                                         
         CLC   MYDBFILE,=C'MPA'    TEST FOR MPA                                 
         BNE   TIME3               DURATIONS ARE CORRECT ON NTI FILE            
         USING MDAYELEM,R6                                                      
         ZIC   R1,MDLTSQH                                                       
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         STC   R1,0(R3)                                                         
         ZIC   R1,MDLTEQH                                                       
         LTR   R1,R1                                                            
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         STC   R1,1(R3)                                                         
         ZIC   R1,MDNUMQH          # OF QH TELECAST                             
         STC   R1,6(R3)                                                         
         B     TIME11A                                                          
*                                                                               
         USING PHELEM,R6                                                        
TIME3    CLC   MYDBFILE,=C'NTI'    TEST FOR NETWORK                             
         BE    TIME3A                                                           
         CLC   MYDBFILE,=C'RLD'    DURATIONS ARE CORRECT ON NET FILES           
         BE    TIME3A                                                           
         SRL   R1,1                CORRECT FOR POSSIBLE ERROR                   
         SLL   R1,1                IN DURATION VALUE                            
TIME3A   STC   R1,6(R3)                                                         
         ZIC   R0,0(R3)                                                         
         AR    R1,R0               R1 HAS END TIME                              
         B     TIME11                                                           
         SPACE 1                                                                
         USING RINVREC,R6                                                       
TIME4    MVC   0(1,R3),RINVKQTR    INV                                          
         ZIC   R1,0(R3)            COMPUTE END 1/4 HR.                          
         AH    R1,DBFACTOR                                                      
         B     TIME11                                                           
         SPACE 1                                                                
TIME6    CLC   =C'RD',0(R6)        DPT FILE?                                    
         BE    TIME6A                                                           
         MVC   0(2,R3),DBACTSQC    TP                                           
         ZIC   R1,1(R3)                                                         
         LA    R1,1(R1)                                                         
         LR    RE,R1               RE=END QHR                                   
         ZIC   R0,DBACTSQC         R0=SQH                                       
         SR    RE,R0               FIND QHR DURATION                            
         STC   RE,6(R3)                                                         
         B     TIME11                                                           
*--------------------------------------------------------------------           
         USING QHELEM,R6                                                        
TIME6A   MVI   ELCODE,QHCODEQ                                                   
         L     R6,DBAQUART                                                      
         BAS   RE,FIRSTEL                                                       
         BNE   XIT                                                              
         MVC   0(2,R3),QHSQH       START AND END QTR HOURS                      
         ZIC   R0,QHSQH                                                         
         ZIC   R1,QHEQH                                                         
         LR    RE,R1                                                            
         SR    RE,R0                                                            
         STC   RE,6(R3)                                                         
         B     TIME11                                                           
*--------------------------------------------------------------------           
TIME8    BAS   RE,SELDPT           SELECT THE DAYPART AND BUILD TIMES           
         B     TIME11A                                                          
         SPACE 1                                                                
TIME11   STC   R1,1(R3)                                                         
TIME11A  LA    R4,0(R3)                                                         
         LA    R5,2(R3)                                                         
         BAS   RE,TIME12                                                        
         LA    R4,1(R3)                                                         
         LA    R5,4(R3)                                                         
         BAS   RE,TIME12                                                        
         CLC   =C'TIMERD',0(R2)                                                 
         BE    TIMERD              LOOKING FOR ROUNDED TIMES                    
         B     XIT                                                              
                                                                                
TIMERD   DS    0H                  RETURN MILITARY ROUNDED TIMES                
         LA    R5,2(R3)            ROUND START TIME                             
         BRAS  RE,RNDTIME                                                       
         LA    R5,4(R3)            ROUND END TIME                               
         BRAS  RE,RNDTIME                                                       
         B     XIT                                                              
                                                                                
                                                                                
TIME12   NTR1                                                                   
         ZIC   R1,0(R4)            CONVERT TO MILITARY                          
         SR    R0,R0                                                            
         D     R0,=F'4'            (HRS IN R1, 1/4'S IN R0)                     
         LA    R1,5(R1)                                                         
         CLI   DBSELMED,C'R'       RADIO IS 5AM BASED                           
         BE    *+8                                                              
         LA    R1,1(R1)            ALL OTHERS 6AM BASED                         
         CHI   R1,24                                                            
         BL    *+8                                                              
         SHI   R1,24                                                            
         MHI   R1,100                                                           
         MHI   R0,15                                                            
         AR    R1,R0                                                            
         STH   R1,0(R5)                                                         
         B     XIT                                                              
         SPACE 2                                                                
         USING RDPTABD,RE                                                       
         USING DRKEY,R6                                                         
SELDPT   NTR1                                                                   
         LARL  RE,RDPTAB                                                        
SELDPT2  CLI   0(RE),0             EOT                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RDPDPT,DRHIQHR      SAME DAYPART                                 
         BNE   *+14                                                             
         CLC   RDPDAY,DRHIGHD      AND DAY                                      
         BE    SELDPT4             SET DATA FIELDS                              
         ZIC   RF,RDPLEN           GET NEXT ENTRY                               
         AR    RE,RF                                                            
         B     SELDPT2                                                          
*                                                                               
SELDPT4  MVC   0(1,R3),RDPQXS      SET START QH                                 
         MVC   1(1,R3),RDPQXE      SET END QH                                   
         ZIC   R0,0(R3)            SET DURATION                                 
         ZIC   R1,1(R3)                                                         
         SR    R1,R0                                                            
         STC   R1,6(R3)                                                         
         XIT1                                                                   
         EJECT                                                                  
*              EXACT TIME EXPRESSIONS FOR INDIVIDUAL DAYS                       
         SPACE 3                                                                
TIMX     XC    0(7,R3),0(R3)                                                    
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    TIMX1                                                            
         B     XIT                                                              
         SPACE 1                                                                
TIMX1    CLI   DBRECTYP,DBRECNTI   TEST FOR 'Q' (NETWK PROG) REC                
         BNE   XIT                                                              
         MVI   ELCODE,X'22'        RUN TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         USING NTELEM,R6                                                        
         LR    R5,R6               SAVE ADDRESS OF 1ST RUN TIME ELEM            
         ZIC   R1,DBACTDAY                                                      
         SRL   R1,4                                                             
         MHI   R1,L'DAYTAB                                                      
         LARL  RF,DAYTAB                                                        
         AR    R1,RF                                                            
TIMX3    ZIC   RF,NTLEN            LOOK FOR MORE RUN TIME ELEMENTS              
         AR    R6,RF                                                            
         CLI   NTCODE,X'22'                                                     
         BNE   TIMX5                                                            
         CLC   NTDAY,0(R1)         HAVE WE FOUND THE EXACT DAY                  
         BNE   TIMX3               IF NOT - TRY NEXT ELEMENT                    
         B     TIMX7               IF YES - USE EXACT DAY ELEMENT               
*                                                                               
TIMX5    LR    R6,R5               RESTORE ADDRESS OF 1ST RUN TIME ELEM         
TIMX7    MVC   0(1,R3),NTSQH                                                    
         ZIC   R1,NTEQH            BUMP EQH SO IT AGREES W PAV EQH              
         LA    R1,1(R1)                                                         
         STC   R1,1(R3)                                                         
         ZIC   R0,NTSQH            GET SQH                                      
         SR    R1,R0               FIND QHR DURATION                            
         STC   R1,6(R3)                                                         
         MVC   2(4,R3),NTSTIM                                                   
         B     XIT                                                              
                                                                                
                                                                                
                                                                                
* LITTLE ROUTINE TO CONVERT MILITARY TIME TO QUARTER HOUR                       
* AT ENTRY,                                                                     
*   MTIME  = MILITARY TIME TO CONVERT                                           
* AT EXIT,                                                                      
*   HOUR   = HOUR # OF TIME,                                                    
*   MINUTE = MINUTES (BETWEEN 0 AND 59),                                        
*   R0     = QUARTER HOUR PASSED BACK TO CALLER                                 
                                                                                
MILTOQHR NTR1                                                                   
         SR    R0,R0               R0 WILL CONTAIN QUARTER HOUR                 
         ST    R0,MINUTE           INITIALIZE MINUTE                            
         ST    R0,HOUR              AND HOUR                                    
*                                                                               
         LHI   R1,600              SET BASE TIME                                
         CLI   DBACTMED,C'R'                                                    
         BNE   *+8                                                              
         LHI   R1,500              RADIO IS BASED ON 5AM                        
*                                                                               
         ZICM  RE,MTIME,(3)                                                     
         CR    RE,R1                                                            
         BNL   *+8                                                              
         AHI   RE,2400                                                          
         SR    RE,R1                                                            
         BZ    MTQ20                                                            
         SRDA  RE,32                                                            
         D     RE,=F'100'                                                       
         STM   RE,RF,MINUTE        SET HOUR & MINUTE                            
         SLA   RF,2                MULTIPLY BY 4                                
         AR    R0,RF                                                            
         SRDA  RE,32                                                            
         D     RE,=F'15'                                                        
         AR    R0,RF                                                            
MTQ20    EQU   *                                                                
         XIT1  REGS=(R0)                                                        
         EJECT                                                                  
*              ALL DAY/TIME EXPRESSIONS (ROTATOR PLUS INDIVIDUAL DAYS)          
         SPACE 3                                                                
ADYTM    XC    0(80,R3),0(R3)      10 BYTES * 8 DAYS (MAXIMUM)                  
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    ADYTM1                                                           
         B     XIT                                                              
         SPACE 1                                                                
ADYTM1   CLI   DBRECTYP,DBRECNTI   TEST FOR 'Q' (NETWK PROG) REC                
         BNE   XIT                                                              
         MVI   ELCODE,X'22'        RUN TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         USING NTELEM,R6                                                        
ADYTM3   MVC   0(1,R3),NTSQH                                                    
         ZIC   R1,NTEQH            BUMP EQH SO IT AGREES W PAV EQH              
         LA    R1,1(R1)                                                         
         STC   R1,1(R3)                                                         
         ZIC   R0,NTSQH            GET SQH                                      
         SR    R1,R0               FIND QHR DURATION                            
         STC   R1,6(R3)                                                         
         MVC   2(4,R3),NTSTIM                                                   
*                                                                               
         LARL  R1,DAYTAB                                                        
ADYTM5   CLI   0(R1),X'FF'         VAR                                          
         BE    ADYTM6                                                           
         CLC   NTDAY,0(R1)                                                      
         BE    ADYTM6                                                           
         LA    R1,L'DAYTAB(R1)                                                  
         B     ADYTM5                                                           
ADYTM6   MVC   7(3,R3),1(R1)       DAY (ALPHA)                                  
*                                                                               
         LA    R3,10(R3)           NEXT OUTPUT ENTRY                            
         ZIC   R1,NTLEN                                                         
         AR    R6,R1                                                            
         CLI   NTCODE,X'22'        TEST FOR INDIVIDUAL DAY ELEMS                
         BE    ADYTM3                                                           
         B     XIT                                                              
         EJECT                                                                  
*              PURE NUMBERS                                                     
         SPACE 3                                                                
PURE     XC    0(3,R3),0(R3)                                                    
         MVC   3(4,R3),=CL16' '                                                 
         CLC   MYDBFILE,=C'PAV'                                                 
         BE    PURE2                                                            
         CLC   MYDBFILE,=C'INV'                                                 
         BE    PURE4                                                            
         CLC   MYDBFILE,=C'IUN'                                                 
         BE    PURE7                                                            
         CLC   MYDBFILE,=C'NAD'                                                 
         BE    PURE6                                                            
         CLC   MYDBFILE,=C'NTI'                                                 
         BNE   XIT                                                              
         CLI   DBRECTYP,DBRECDEM   TEST FOR PAV RECORD                          
         BNE   XIT                                                              
         SPACE 1                                                                
         USING PRKEY,R6                                                         
PURE2    MVC   0(2,R3),PRKMINOR    PAV                                          
         EDIT  (1,0(R3)),(2,3(R3)),FILL=0                                       
         ZIC   R1,PRDW             (DAY)                                        
         SRL   R1,4                                                             
         STC   R1,5(R3)                                                         
         MVI   DUB,X'F0'           MASK FOR DAY=0-9                             
         CLI   5(R3),10            TEST DAY DIGIT OR HEX                        
         BL    PURE3               DIGIT                                        
         SHI   R1,9                TURN HEX VALUE INTO LETTER                   
         STC   R1,5(R3)                                                         
         MVI   DUB,X'C0'                                                        
         SPACE 1                                                                
PURE3    OC    5(1,R3),DUB                                                      
         ZIC   R1,PRDW             (WEEK)                                       
         SLL   R1,28                                                            
         SRL   R1,29                                                            
         STC   R1,6(R3)                                                         
         OI    6(R3),X'F0'                                                      
         CLI   6(R3),X'F0'                                                      
         BNE   *+8                                                              
         MVI   6(R3),C' '                                                       
         TM    PRDW,X'01'                                                       
         BNO   XIT                                                              
         NI    6(R3),X'CF'         CHANGE 1-7 TO A-G                            
         CLI   6(R3),C' '                                                       
         BNE   XIT                                                              
         MVI   6(R3),X'F0'                                                      
         B     XIT                                                              
         SPACE 1                                                                
         USING RINVKEY,R6                                                       
PURE4    MVC   0(3,R3),RINVKINV                                                 
         EDIT  (1,0(R3)),(2,3(R3)),FILL=0                                       
         MVC   5(2,R3),RINVKINV+1                                               
         B     XIT                                                              
         SPACE 1                                                                
         USING PRKEY,R6                                                         
PURE6    DS    0H                                                               
         CLI   SPNTFLG,0                                                        
         BE    *+14                                                             
         CLC   DBFILE,=C'PAV'                                                   
         BE    PURE6A                                                           
         EDIT  PRKMINOR,(4,3(R3))                                               
         B     XIT                                                              
PURE6A   L     RF,DBCOMFCS                                                      
         L     RF,CHEXOUT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,PRKMINOR,3(R3),2                                       
         B     XIT                                                              
         SPACE 1                                                                
         USING RINVKEY,R6                                                       
PURE7    CLI   RINVKINV+3,0        OLD FRMT INVENTORY                           
         BE    PURE4                                                            
         MVC   3(4,R3),RINVKINV    NEW FRMT INVENTORY                           
         B     XIT                                                              
         EJECT                                                                  
*              NTI NUMBERS                                                      
         SPACE 3                                                                
NTI      MVC   0(4,R3),=CL16' '                                                 
         CLC   MYDBFILE,=C'NAD'                                                 
         BE    NTI1                                                             
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    NTI1                                                             
         CLC   MYDBFILE,=C'RLD'                                                 
         BE    NTI1                                                             
         CLC   MYDBFILE,=C'MPA'                                                 
         BE    PRGCODE                                                          
         CLC   MYDBFILE,=C'PAV'                                                 
         BE    NTIPAV                                                           
         CLC   MYDBFILE,=C'TP '                                                 
         BNE   XIT                                                              
         L     R6,DBAQUART                                                      
         MVI   ELCODE,X'21'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   XIT                                                              
         B     NTI5                                                             
NTI1     L     RE,DBAREC                                                        
         USING PMKEY,RE                                                         
         CLI   PMCODE,MXMCODEQ     C'M' MINUTE BY MINUTE DATA                   
         BE    NTI10                                                            
         CLI   PMCODE,PMCODEQU     C'Q' PROGRAM DATA                            
         BNE   NTI05                                                            
         CLI   PMMEDIA,C'C'        CABLE                                        
         BE    *+12                                                             
         CLI   PMMEDIA,C'c'        CABLE COMMERCIAL AVERAGE                     
         BNE   NTI05                                                            
         CLI   PMBTYP,C'C'         MONITOR PLUS IS LIKE NETWORK                 
         BNE   NTI10               OTHERWISE CABLE FORMAT                       
NTI05    MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PHELEM,R6                                                        
         SR    R0,R0                                                            
         ICM   R0,3,PHPNUM         EDIT THINKS IT'S SIGNED                      
         EDIT  (R0),(5,0(R3)),FILL=0                                            
         B     XIT                                                              
*                                                                               
NTI10    MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PHTELEM,R6                                                       
         SR    R0,R0                                                            
         ICM   R0,3,PHTDDS         EDIT THINKS IT'S SIGNED                      
         EDIT  (R0),(5,0(R3)),FILL=0                                            
         B     XIT                                                              
*                                                                               
         SPACE 3                                                                
NTIL     MVC   0(4,R3),=CL16' '                                                 
         CLC   MYDBFILE,=C'NAD'                                                 
         BE    NTIL2                                                            
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    NTIL2                                                            
         CLC   MYDBFILE,=C'RLD'                                                 
         BE    NTIL2                                                            
NTIL1    MVC   0(10,R3),=C'   N/A    '                                          
         B     XIT                                                              
*                                                                               
NTIL2    L     RE,DBAREC                                                        
         USING PMKEY,RE                                                         
         CLI   PMCODE,MXMCODEQ     C'M' MINUTE BY MINUTE DATA                   
         BE    NTIL4                                                            
         CLI   PMCODE,PMCODEQU     C'Q' PROGRAM DATA                            
         BNE   NTIL1                                                            
         CLI   PMMEDIA,C'C'        CABLE                                        
         BE    NTIL6                                                            
         CLI   PMMEDIA,C'c'        CABLE COMMERCIAL AVERAGE                     
         BE    NTIL6                                                            
         DROP  RE                                                               
NTIL4    MVI   ELCODE,PHTCODEQ     X'10' ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PHTELEM,R6                                                       
         CLI   PHTLEN,PHTLNEQ                                                   
         BL    NTIL4A                                                           
         MVO   DUB(6),PHTNTI                                                    
         OI    DUB+5,X'0F'                                                      
         EDIT  (P6,DUB),(10,0(R3)),FILL=0                                       
         B     XIT                                                              
*                                                                               
NTIL4A   L     RE,DBAREC           OLDER RECORDS DON'T HOLD NTILONG             
         USING PMKEY,RE            USE NTI NUMBER INSTEAD                       
         EDIT  PMPNUM,(10,0(R3)),FILL=0                                         
         B     XIT                                                              
         DROP  RE                                                               
*                                                                               
NTIL6    DS    0C                  ALWAYS GET PROGRAM NUM                       
         ICM   RE,15,DBSPANAD                                                   
         BZ    XIT                                                              
         MVC   0(10,R3),94(RE)                                                  
         B     XIT                                                              
PRGCODE  DS    0H                                                               
         USING MRKEY,R6                                                         
         SR    R0,R0                                                            
         ICM   R0,3,MRPNUM         EDIT THINKS IT'S SIGNED                      
         EDIT  (R0),(5,0(R3)),FILL=0                                            
         B     XIT                                                              
*                                                                               
*-------------------------- MYDBFILE=PAV -----------------------------          
*                                                                               
NTIPAV   DS    0H                                                               
         MVI   ELCODE,PHCODEQ      ELEMENT CODE                                 
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PHELEM,R6                                                        
         SR    R0,R0                                                            
         ICM   R0,3,PHPNUM         ASSUME PRE-REVISION                          
         CLI   PHREVID,X'FF'       IS THIS A REVISION?                          
         BNE   NTIPAVX             NO                                           
         ICM   R0,7,PHPNUM3        YES: GET 3-BYTE PROGRAM NUMBER               
NTIPAVX  EDIT  (R0),(5,0(R3)),FILL=0                                            
         B     XIT                                                              
         DROP  R6                                                               
*-----------------------------------------------------------------              
         USING QIELEM,R6                                                        
NTI5     CLI   QIELN,4                                                          
         BNE   NTI6                                                             
         NI    QIPNUM,X'7F'                                                     
         SR    R1,R1                                                            
         ICM   R1,3,QIPNUM                                                      
         L     R6,DBAREC                                                        
         AR    R6,R1                                                            
*                                                                               
NTI6     SR    R0,R0                                                            
         ICM   R0,7,QIPNUM                                                      
         EDIT  (R0),(5,0(R3)),FILL=0                                            
         B     XIT                                                              
*-----------------------------------------------------------------              
*  TP NSI PROGRAM NUMBERS ONLY                                                  
TPNO     DS    0H                                                               
         MVI   GOSUBN,TPNO#                                                     
         GOTO1 AGOSUB                                                           
         J     XIT                                                              
         EJECT                                                                  
*------------------- PROGRAM AVERAGE PROGRAM NUMBER ------------------*         
                                                                                
PVNO     DS    0H                                                               
         MVI   GOSUBN,PVNO#                                                     
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         J     XIT                                                              
         EJECT                                                                  
*           GET TP NSI AFFILIATES                                               
AFFL     MVC   0(5,R3),=CL5'N/A'                                                
         CLC   MYDBFILE,=C'TP '                                                 
         BNE   XIT                                                              
         CLI   DBSELSRC,C'N'                                                    
         BNE   XIT                                                              
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING QIELEM,R6                                                        
         MVC   0(5,R3),QIAFFIL                                                  
         B     XIT                                                              
*        GET GAA FLAG                                                           
GAA      MVI   0(R3),C'N'          DEFAULT                                      
         CLI   DBSELSTA+4,C'T'                                                  
         BE    *+8                                                              
         CLI   DBSELSTA+4,C'M'                                                  
         BE    *+8                                                              
         CLI   DBSELSTA+4,C'S'                                                  
         BNE   XIT                                                              
         MVI   ELCODE,X'55'        CHECK FOR GAA ELEMENTS                       
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVI   0(R3),C'Y'          SET FLAG IF GAA                              
         B     XIT                                                              
         SPACE 2                                                                
* CALCULATE THE NETWORK                                                         
NET      CLC   =C'MPA',MYDBFILE                                                 
         BE    NET03                                                            
         CLC   =C'TP',MYDBFILE                                                  
         BE    NET50                                                            
         B     XIT                                                              
NET03    CLI   2(R6),C'N'                                                       
         BE    NET05                                                            
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   0(3,R3),3(R6)                                                    
         B     XIT                                                              
         USING MRKEY,R6                                                         
NET05    SR    R1,R1                                                            
         ICM   R1,3,MRPNUM                                                      
         CHI   R1,9600             DAYPART PROGRAMS?                            
         BH    NET20               YES, USE 2ND TABLE                           
*                                                                               
         LARL  RE,NETTABL1                                                      
         USING NETRANGE,RE                                                      
NET10    CLI   NETMIN,X'FF'                                                     
         BE    XIT                                                              
         CH    R1,NETMIN                                                        
         BL    XIT                                                              
         CH    R1,NETMAX                                                        
         BNH   NETUSE                                                           
         LA    RE,NETLEN(RE)                                                    
         B     NET10                                                            
*                                                                               
NETUSE   MVC   0(4,R3),NETCALL                                                  
         B     XIT                                                              
*                                                                               
NET20    MVC   0(4,R3),=CL4' '                                                  
         SHI   R1,9600                                                          
         LARL  RE,NETTABL2                                                      
         B     NET10                                                            
*                                                                               
*--DPT-------------------------------------------------------------             
NET50    MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING DIELEM,R6                                                        
         MVC   0(3,R3),DIAFFL      GET AFFLIATES AS NET                         
         B     XIT                                                              
         DROP  R6                                                               
         DROP  RE                                                               
         EJECT                                                                  
*              DAY EXPRESSIONS                                                  
         SPACE 3                                                                
DAY      DS    0H                                                               
         LARL  RF,DAYNTR                                                        
         BASR  RE,RF                                                            
         B     XIT                                                              
         EJECT                                                                  
* MARKET NUMBER                                                                 
         SPACE 1                                                                
MAR      DS    0H                                                               
         CLC   MYDBFILE,=C'CTP'     FOR COUNTY COVERAGE,                        
         BNE   MAR_0                REDIRECT TO DMA MARKET ROUTINE              
         MVC   0(4,R2),=C'DMA#'                                                 
         LARL  RF,DMAINFO                                                       
         BASR  RE,RF                                                            
         B     XIT                                                              
*                                                                               
MAR_0    CLC   MYDBFILE,=C'MPA'                                                 
         BNE   MAR_1                                                            
         USING MRKEY,R6                                                         
         MVC   0(2,R3),MRKMKT                                                   
         CLI   DBSELSRC,C'N'                                                    
         BNE   XIT                                                              
         SR    R1,R1                                                            
         ICM   R1,3,MRKMKT                                                      
         SHI   R1,400                                                           
         STCM  R1,3,0(R3)                                                       
         B     XIT                                                              
MAR_1    MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BE    MAR_2                                                            
         MVC   0(2,R3),=H'9999'                                                 
         B     XIT                                                              
*                                                                               
         USING MARELEM,R6                                                       
MAR_2    MVC   0(2,R3),MARNO                                                    
         B     XIT                                                              
         SPACE 2                                                                
* MARKET NAME (FROM MARKET NAME RECORDS)                                        
         SPACE 1                                                                
MNAME    XC    0(2,R3),0(R3)                                                    
         MVI   2(R3),C' '                                                       
         MVC   3(29,R3),2(R3)                                                   
         CLC   MYDBFILE,=C'RDP'                                                 
         BE    *+10                                                             
         CLC   =C'TP',MYDBFILE                                                  
         BNE   XIT                                                              
         CLI   DBRECTYP,DBRECMK    TEST FOR MARKET RECORDS                      
         BNE   XIT                                                              
         MVI   ELCODE,DMECODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DMELEM,R6                                                        
         MVC   0(2,R3),DMMNO       MARKET NUMBER (BINARY)                       
         ZIC   R1,DMLEN                                                         
         SHI   R1,DMMNAME-DMELEM+1                                              
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   2(0,R3),DMMNAME     MARKET NAME                                  
*                                  DEAL WITH HISPANIC SURVEY MARKETS            
*                                  PRE-NOV11, NHSI IS NO MORE NOV11+            
         CLC   DBSELBK,=AL2(NOV_11)                                             
         JNL   XIT                                                              
         CLI   DBBTYPE,C'J'                                                     
         JE    *+12                                                             
         CLI   DBBTYPE,C'H'                                                     
         JNE   XIT                                                              
         LARL  RE,NHSITAB                                                       
NHSIMN   CLI   0(RE),X'FF'                                                      
         JE    XIT                                                              
         CLC   DMMNO,0(RE)                                                      
         JE    *+12                                                             
         LA    RE,4(RE)                                                         
         J     NHSIMN                                                           
         MVC   2(5,R3),=C'NHSI ' ADD A CAPTION FOR nhsi MARKETS                 
         EX    R1,*+8                                                           
         J     XIT                                                              
         MVC   7(0,R3),DMMNAME     SEND SHIFTED MARKET NAME                     
         DROP  R6                                                               
         EJECT                                                                  
* NTI PROGRAM TYPE                                                              
         SPACE 1                                                                
TYPE     DS    0H                                                               
         MVC   0(4,R3),=CL16' '                                                 
         CLC   MYDBFILE,=C'IUN'                                                 
         BE    TYPEIUN                                                          
         CLC   MYDBFILE,=C'EVN'                                                 
         BE    TYPEEVN                                                          
         CLC   MYDBFILE,=C'NAD'                                                 
         BE    TYPENET                                                          
         CLC   MYDBFILE,=C'RLD'                                                 
         BE    TYPENET                                                          
         CLC   MYDBFILE,=C'NTI'                                                 
         BNE   XIT                                                              
TYPENET  MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PHTELEM,R6                                                       
         MVC   0(2,R3),PHTPTYP     PROGRAM TYPE                                 
         MVI   2(R3),C'R'          REGULAR/SPECIAL                              
         TM    PHTDTYP,X'01'       TEST FOR REGULAR PROGRAM                     
         BO    *+8                 YES                                          
         MVI   2(R3),C'S'          NO-SET SPECIAL                               
         TM    PHTDTYP,X'05'       TEST FOR REG+SPC PROGRAM                     
         BNO   *+8                 YES                                          
         MVI   2(R3),C'T'          NO-SET TOTAL                                 
         MVI   3(R3),C'O'          ORIGINAL/REPEAT                              
         TM    PHTDTYP,X'08'       TEST FOR ORIGINAL                            
         BO    *+8                 YES                                          
         MVI   3(R3),C'R'          NO-SET REPEAT                                
         TM    PHTDTYP,X'18'       COMBINED (SYND ONLY)                         
         BNO   *+8                                                              
         MVI   3(R3),C'C'                                                       
         TM    PHTDTYP,X'20'       MULTIPLE PROGRAMS (SYND ONLY)                
         BZ    *+8                                                              
         MVI   3(R3),C'M'                                                       
         B     XIT                                                              
*                                                                               
TYPEIUN  DS    0H                                                               
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING RINVCEL,R6                                                       
         MVC   0(2,R3),RINVCODE                                                 
         B     XIT                                                              
*                                                                               
TYPEEVN  DS    0H                                                               
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NPGEL03,R6                                                       
         MVC   0(2,R3),NPPRGTYP    PROGRAM TYPE FROM PROGRAM RECORD             
         B     XIT                                                              
*                                                                               
PTYPE    CLC   MYDBFILE,=C'NAD'                                                 
         BE    TYPE                                                             
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    TYPE                                                             
         CLC   MYDBFILE,=C'RLD'                                                 
         BE    TYPE                                                             
         CLC   MYDBFILE,=C'EVN'                                                 
         BE    TYPE                                                             
         CLC   MYDBFILE,=C'TP '    PROGRAM TYPE FOR NSI TP                      
         BNE   XIT                                                              
         CLI   DBSELSRC,C'N'                                                    
         BNE   XIT                                                              
         MVI   ELCODE,X'21'                                                     
         L     R6,DBAQUART                                                      
         BAS   RE,FIRSTEL                                                       
         BNE   XIT                                                              
         USING QIELEM,R6                                                        
         CLI   QIELN,4             DO WE HAVE A DUP?  4 BYTES LONG              
         BNE   PTYPE2              NO, USE THIS TYPE                            
         NI    QIPNUM,X'7F'                                                     
         SR    R1,R1                                                            
         ICM   R1,3,QIPNUM         GRAB THE DISPLACEMENT FROM REC               
         L     RF,DBAREC                                                        
         AR    RF,R1                                                            
         LR    R6,RF                                                            
PTYPE2   MVC   0(2,R3),QIPTYPE                                                  
         B     XIT                                                              
*                                                                               
PTYP4    MVC   0(8,R3),=C'N/A N/A '                                             
         CLC   MYDBFILE,=C'EVN'                                                 
         BE    PTYP4EVN                                                         
         L     RE,DBAREC                                                        
         USING PMKEY,RE                                                         
         CLI   PMCODE,MXMCODEQ     C'M' MINUTE BY MINUTE DATA                   
         BE    *+12                                                             
         CLI   PMCODE,PMCODEQU     C'Q' PROGRAM DATA                            
         BNE   XIT                                                              
         CLI   PMMEDIA,C'N'        NETWORK                                      
         BE    PTYP4A              OR                                           
         CLI   PMMEDIA,C'n'        NETWORK COMMERCIAL AVERAGE                   
         BE    PTYP4A              OR                                           
         CLI   PMMEDIA,C'C'        CABLE                                        
         BE    PTYP4A              OR                                           
         CLI   PMMEDIA,C'c'        CABLE COMMERCIAL AVERAGE                     
         BNE   XIT                                                              
         DROP  RE                                                               
PTYP4A   MVI   ELCODE,PHTCODEQ     X'10' ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PHTELEM,R6                                                       
         CLI   PHTLEN,PHTLNEQ                                                   
         BL    PTYP4C                                                           
         OC    PHTPTYP4,PHTPTYP4                                                
         BZ    PTYP4C                                                           
         MVC   0(4,R3),PHTPTYP4                                                 
         MVC   4(4,R3),PHTSPTYP                                                 
         B     XIT                                                              
*                                                                               
PTYP4C   CLI   PHTPTYP,C'S'                                                     
         BNE   XIT                                                              
         CLI   1(R6),X'15'                                                      
         BL    XIT                                                              
         XC    DUB,DUB                                                          
         MVO   DUB+2(6),PHTNTI                                                  
         OI    DUB+7,X'0F'                                                      
         CVB   RF,DUB                                                           
         STCM  RF,7,DUB                                                         
         LARL  RF,NETSUBP                                                       
PTYP4D   CLI   0(RF),X'FF'                                                      
         BE    PTYP4E                                                           
         CLC   DUB(3),0(RF)                                                     
         BE    *+12                                                             
         LA    RF,7(RF)                                                         
         B     PTYP4D                                                           
         MVC   4(4,R3),3(RF)                                                    
         B     XIT                                                              
*                                                                               
PTYP4E   LARL  RF,SYNSUBP                                                       
PTYP4F   CLI   0(RF),X'FF'                                                      
         BE    XIT                                                              
         CLC   DUB(3),0(RF)                                                     
         BE    *+12                                                             
         LA    RF,7(RF)                                                         
         B     PTYP4F                                                           
         MVC   4(4,R3),3(RF)                                                    
         B     XIT                                                              
*                                                                               
PTYP4EVN DS    0H                                                               
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING NPGEL03,R6                                                       
         MVC   4(4,R3),NPPRGSTP    SUB-PROGRAM TYPE FROM PROGRAM RECD           
         B     XIT                                                              
*                                                                               
PSOUR    CLC   MYDBFILE,=C'TP '    PROGRAM SOURCE FOR NSI TP                    
         BNE   XIT                                                              
         CLI   DBSELSRC,C'N'                                                    
         BNE   XIT                                                              
         MVI   ELCODE,X'21'                                                     
         L     R6,DBAQUART                                                      
         BAS   RE,FIRSTEL                                                       
         BNE   XIT                                                              
         USING QIELEM,R6                                                        
         CLI   QIELN,4             DO WE HAVE A DUP?  4 BYTES LONG              
         BNE   PSOUR2              NO, USE THIS TYPE                            
         NI    QIPNUM,X'7F'                                                     
         SR    R1,R1                                                            
         ICM   R1,3,QIPNUM         GRAB THE DISPLACEMENT FROM REC               
         L     RF,DBAREC                                                        
         AR    RF,R1                                                            
         LR    R6,RF                                                            
PSOUR2   MVC   0(2,R3),QIPRSRC                                                  
         B     XIT                                                              
         EJECT                                                                  
WEEK     MVI   GOSUBN,WEEK#                                                     
         GOTO1 AGOSUB                                                           
         B     XIT                                                              
         EJECT                                                                  
* TOTAL PROGRAM DURATION (PAV FILE ONLY)                                        
*                                                                               
TOT2DUR  DS    0H                                                               
         MVI   1(R3),0                 2ND BYTE OF 2-BYTE DURATION              
*                                                                               
TOTDUR   MVI   0(R3),0                                                          
         CLC   DBFILE,=C'PAV'                                                   
         BNE   XIT                                                              
         MVI   ELCODE,PHCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PHELEM,R6                                                        
         CLI   PHREVID,X'FF'       IS A REVISION ID PRESENT?                    
         BNE   TOTDUR1                                                          
         CLI   PHREV,3             YES: DOES IT HAVE FIELD PHDURTO2 ?           
         BL    TOTDUR1                                                          
         CLC   =C'TOT2DUR',0(R2)   YES: 2-BYTE DURATION REQUESTED?              
         BNE   TOTDUR1                                                          
         MVC   0(2,R3),PHDURTO2    YES: EXTRACT TOTAL DURATION (2-BYTE)         
         B     XIT                                                              
*                                                                               
TOTDUR1  DS    0H                                                               
         CLI   PHELN,PHDURTOT-PHELEM+1 TEST FOR EXTENDED ELEMENT                
         BL    TOTDUR2             NO-DURATION MUST BE CALCULATED               
         CLI   PHDURTOT,0          TEST FOR BAD RATING SERVICE RECORD           
         BE    TOTDUR2             YES                                          
*                                                                               
         LR    RE,R3                                                            
         CLC   =C'TOT2DUR',0(R2)   2-BYTE DURATION REQUESTED?                   
         BNE   *+8                                                              
         AHI   RE,1                YES: MUST STORE 1-BYTE VALUE IN LOB          
         MVC   0(1,RE),PHDURTOT    EXTRACT TOTAL DURATION                       
         B     XIT                                                              
*                                                                               
TOTDUR2  ZIC   R1,PHDUR                                                         
         CLI   DBSELMED,C'N'                                                    
         BE    *+12                DON'T CUT OFF ODD QHRS FOR NETWORK           
         SRL   R1,1                                                             
         SLL   R1,1                                                             
         ST    R1,DUB              SAVE QUARTER HOUR DURATION                   
*                                                                               
         ZIC   RF,PHDWKS           GET WEEK BITS                                
         SLL   RF,28               ISOLATE THEM                                 
         SR    RE,RE                                                            
         LTR   RF,RF                                                            
         BNZ   *+8                                                              
         LA    R0,4                COUNTER                                      
         SR    R1,R1               R1=N'WEEKS                                   
*                                                                               
TOTDUR3  SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         LA    R1,1(R1)            INCREMENT WEEK COUNT                         
         SR    RE,RE                                                            
         BCT   R0,TOTDUR3                                                       
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         CLI   DBSELMED,C'N'                                                    
         BNE   *+8                                                              
         LA    R1,1                DEFAULT TO ONE WEEK ACTIVE                   
*                                                                               
         CLI   DBSELSRC,C'A'       CANNOT DETERMINE START WEEK                  
         BNE   TOTDUR4             FOR ARB BEFORE MAY86                         
         CLC   DBSELBK,=AL2(MAY_86) FUDGE TO CORRECT IT                         
         BNL   TOTDUR4                                                          
         CLI   PHDWKS,X'02'                                                     
         BNE   *+8                                                              
         LA    R1,3                                                             
*                                                                               
TOTDUR4  DS    0H                                                               
         ST    R1,DUB+4            SAVE NUMBER OF WEEKS                         
         L     R6,DBAREC                                                        
         USING PRKEY,R6                                                         
         ZIC   RE,PRDW                                                          
         CLI   PRCODE,PMCODEQU     C'Q' PROGRAM DATA                            
         BNE   TOTDUR5                                                          
         CLI   PRMEDIA,C'N'        NETWORK                                      
         BE    *+12                                                             
         CLI   PRMEDIA,C'n'        NETWORK COMMERCIAL AVERAGE                   
         BNE   TOTDUR5                                                          
         MVI   ELCODE,NTCODEQU                                                  
         BAS   RE,GETEL                                                         
         USING NTELEM,R6                                                        
         SR    R0,R0                R0 COUNTS # OF DAYS                         
         ZICM  RF,NTDAY,(8)                                                     
         SLL   RF,1                HIGH-ORDER-BIT NOT USED IN DAY CODE          
TOTDUR4B LTR   RF,RF                                                            
         BZ    TOTDUR4X                                                         
         SR    RE,RE                                                            
         SLDL  RE,1                                                             
         AR    R0,RE                                                            
         B     TOTDUR4B                                                         
TOTDUR4X LR    R1,R0                                                            
         SR    R0,R0                                                            
         B     TOTDUR6                                                          
*                                                                               
TOTDUR5  SRL   RE,4                                                             
         MHI   RE,L'DAYTAB         INDEX INTO DAY TABLE                         
         LARL  R1,DAYTAB                                                        
         AR    RE,R1                                                            
         ZIC   R1,4(RE)            EXTRACT NUMBER OF DAYS FROM TABLE            
*                                                                               
TOTDUR6  M     R0,DUB              MULTIPLY BY N'QUARTER HOURS                  
         M     R0,DUB+4            MULTIPLY BY N'WEEKS                          
*                                                                               
         LR    RE,R3                                                            
         CLC   =C'TOT2DUR',0(R2)   2-BYTE DURATION REQUESTED?                   
         BNE   *+8                                                              
         AHI   RE,1                YES: MUST STORE 1-BYTE VALUE IN LOB          
         STC   R1,0(RE)                                                         
         B     XIT                                                              
         EJECT                                                                  
*              CORRECTION INFORMATION                                           
         SPACE 3                                                                
CORRECT  MVC   0(16,R3),=CL16' '                                                
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    CORR2                                                            
         B     XIT                                                              
         SPACE 1                                                                
CORR2    MVI   ELCODE,X'25'        PAVFILE                                      
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING CRRELEM,R6                                                       
         MVI   0(R3),C'A'                                                       
         TM    CRRTYPE,X'01'       BIT 7 - 0=ADD 1=CHANGE                       
         BZ    *+8                                                              
         MVI   0(R3),C'C'                                                       
         MVC   1(2,R3),CRROBOOK    ORIGINATING BOOK YYWW                        
         B     XIT                                                              
         DROP  R6                                                               
*              CORRECTION INFORMATION FOR RLD                                   
         SPACE 3                                                                
CORAMRLD XC    0(6,R3),0(R3)                                                    
         CLC   MYDBFILE,=C'RLD'                                                 
         BNE   XIT                                                              
         MVI   ELCODE,CRRCODEQ     X'25' CORRECTION ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
*&&DO                                                                           
* JAN/2021: COMMENTED OUT BY DEIS.                                              
*  DEIS DISCOVERED THAT THIS CODE HAS NEVER BEEN EXECUTED, DUE TO A BUG         
*  IN DEMXMI WHICH PREVENTED THE CREATION OF *ANY* MXM CORRECTION               
*  ELEMENTS. THAT BUG WILL BE FIXED, SO THAT GOING FORWARD, WE WILL             
*  BUILD THE ELEMENTS CORRECTLY. BUT MARIA DOES NOT WANT TO EXPOSE THAT         
*  FIX TO CLIENTS (AT LEAST FOR NOW), BECAUSE:                                  
*   A) NO ONE HAS EVER COMPLAINED ABOUT IT, AND                                 
*   B) IF WE FIX IT, THEN CLIENTS MIGHT NOTICE THE SUDDEN PRESENCE OF           
*      THIS DATA IN REPORTS, AND THEY MIGHT WANT US TO FIX THE PROBLEM          
*      RETROACTIVELY FOR HISTORICAL DATA. WE'D RATHER NOT BE FORCED TO          
*      DO THAT.                                                                 
*  THEREFORE: THE CODE BELOW IS BEING COMMENTED OUT, AND IF WE EVER             
*  DECIDE TO EXPOSE THESE FIELDS, WE SHOULD UNCOMMENT THIS CODE. THAT           
*  WILL FIX THE PROBLEM FOR ANY MXM DATA LOADED AFTER THE DEMXMI FIX IS         
*  INSTALLED.                                                                   
         USING CRRELEM,R6                                                       
         CLI   CRRLEN,CRRLEN2Q                                                  
         BL    XIT                                                              
         CLI   CRRRTYP,CRRTYPDS    CHANGE TO DESCRIPTIVE DATA                   
         BNE   *+10                                                             
         MVC   0(4,R3),=C'DESC'                                                 
         CLI   CRRRTYP,CRRTYPVW    CHANGE TO VIEWING DATA                       
         BNE   *+10                                                             
         MVC   0(4,R3),=C'VIEW'                                                 
         CLI   CRRRTYP,CRRTYPNA    CHANGE NOT IDENTIFIED                        
         BNE   *+10                                                             
         MVC   0(4,R3),=C'UNKN'                                                 
         L     RF,DBCOMFCS                                                      
         L     RF,CDATCON-COMFACSD(,RF)                                         
         GOTO1 (RF),DMCB,(3,CRRDATE),(2,4(R3)),0   REPROCESSING DATE            
         DROP  R6                                                               
*&&                                                                             
         B     XIT                                                              
         EJECT                                                                  
COVER    DS    0H                  NETWORK COVERAGE FACTOR                      
         XC    0(2,R3),0(R3)                                                    
         CLC   MYDBFILE,=C'NTI'    NTI                                          
         BE    *+14                                                             
         CLC   MYDBFILE,=C'RLD'    AND RLD                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PHTELEM,R6                                                       
         MVC   0(2,R3),PHTCOVR                                                  
         B     XIT                                                              
         SPACE 2                                                                
SCOUNT   DS    0H                  NETWORK STATION COUNT                        
         XC    0(2,R3),0(R3)                                                    
         CLC   MYDBFILE,=C'NTI'    NTI                                          
         BE    *+14                                                             
         CLC   MYDBFILE,=C'RLD'    AND RLD                                      
         BNE   XIT                                                              
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PHTELEM,R6                                                       
         MVC   0(2,R3),PHTSCNT                                                  
         B     XIT                                                              
         SPACE 2                                                                
DAYPART  DS    0H                  BINARY DAY/DAYPART                           
         USING DRKEY,R6            1=DAY, 2=DAYPART NUMBER                      
         XC    0(2,R3),0(R3)                                                    
         CLC   MYDBFILE,=C'RDP'    RADIO DAYPART ONLY                           
         BNE   XIT                                                              
         MVC   0(1,R3),DRHIGHD                                                  
         MVC   1(1,R3),DRHIQHR                                                  
         B     XIT                                                              
         SPACE 3                                                                
STATION  DS    0H                                                               
         XC    0(4,R3),0(R3)                                                    
         CLC   MYDBFILE,=C'MPA'                                                 
         BE    STATN05                                                          
         CLC   =C'TP',MYDBFILE                                                  
         BNE   XIT                                                              
         USING DRKEY,R6                                                         
         MVC   0(4,R3),DRSTAT                                                   
         CLC   =C'****',DRSTAT                                                  
         BNE   XIT                                                              
         MVC   0(4,R3),=C'N/A '                                                 
         DROP  R6                                                               
         B     XIT                                                              
STATN05  MVC   0(4,R3),=C'*US*'    DEFAULT TO NOT AVAILABLE                     
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         MVC   0(4,R3),2(R6)                                                    
         CLC   =C'****',2(R6)      STATION NOT AVAILABLE?                       
         BNE   *+10                YEP, USED ALREADY                            
         MVC   0(4,R3),=C'N/A '                                                 
         CLI   0(R3),C' '          FUDGE STATION FOR TOTAL US                   
         BE    *+8                                                              
         CLI   0(R3),0                                                          
         BNE   XIT                                                              
         MVC   0(4,R3),=C'*US*'                                                 
         B     XIT                                                              
         EJECT                                                                  
*--------------- ALPHA AND NUMERIC MARKET TRANSLATIONS ---------------*         
                                                                                
* NOTE: R7 (=DBAQUART) WILL BE CLOBBERED HERE                                   
                                                                                
ALFNUMMK DS    0H                                                               
         XC    DUB,DUB             SPECIAL CALL FOR SYSFACS                     
         MVC   DUB(4),=XL4'FEFFFFFF'                                            
         L     R6,DBCOMFCS                                                      
         USING COMFACSD,R6                                                      
         ICM   RF,15,CSWITCH                                                    
         BNZ   *+16                                                             
         L     RE,CMASTC                                                        
         L     RF,MCSSB-MASTD(RE)  OBTAIN ALET FOR TABS DSPACE                  
         B     ANM05                                                            
*                                                                               
         GOTO1 (RF),DUB            GOTO V(SWITCH)                               
         L     RF,0(R1)            RF=V(SYSFACS)                                
         L     RF,VSSB-SYSFACD(RF) RF=V(SSB)                                    
*                                                                               
ANM05    DS    0H                                                               
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
         L     R5,=A(DEMTABCL)     A(LIST OF TABLES WITHIN PROGRAM)             
         A     R5,RELO                                                          
         USING DEMTABCL,R5                                                      
         MVC   WORK(TABLISTQ),TABLIST                                           
         OC    WORK+1(3),WORK+1    DO WE HAVE A(ALPHAMKT) TABLE?                
         BNZ   ANM08               YES                                          
*                                                                               
         GOTO1 CDEMADDR,DMCB,(X'FF',WORK),(R6)                                  
*                                                                               
         ICM   RF,15,CPROTOFF                                                   
         JZ    *+6                                                              
         BASR  RE,RF               *** TURN STORAGE PROTECTION OFF ***          
*                                                                               
         MVC   TABLIST(TABLISTQ),WORK     SAVE A(TABLES) IN CSECT               
*                                                                               
         ICM   RF,15,CPROTON                                                    
         JZ    *+6                                                              
         BASR  RE,RF               *** STORAGE PROTECTION BACK ON ***           
*                                                                               
         DROP  R6                                                               
         DROP  R5                                                               
*                                                                               
ANM08    DS    0H                                                               
         LAM   AR2,AR2,=F'0'                                                    
         ICM   R6,15,WORK          R6-->ALPHAMKT TABLE                          
         BZ    ANMXX                                                            
         LAM   AR6,AR6,ALET                                                     
         SAC   512                                                              
         USING DAMHDRD,R6                                                       
                                                                                
         LA    R2,0(R2)            CLEAR HOB                                    
         SAM31                     SWITCH TO 31-BIT MODE                        
*                                                                               
ANM10    CLC   0(2,R6),=X'FFFF'    AT END OF TABLE?                             
         BE    ANMXSM                                                           
         CLC   DAMHMED,DBINTMED    MATCH AGAINST MEDIA,                         
         BNE   ANM12                                                            
         CLC   DAMHSRC,DBACTSRC     AND SOURCE                                  
         BE    ANM20                                                            
         CLI   DBACTSRC,C'T'       FOR TRITON,                                  
         BNE   ANM12                                                            
         CLI   DAMHSRC,C'A'        LOOK AT ARBITRON MARKETS                     
         BE    ANM20                                                            
*                                                                               
ANM12    ICM   R6,15,DAMHAET       BUMP TO NEXT MEDIA/SOURCE SECTION            
         AHI   R6,1                                                             
         B     ANM10                                                            
*                                                                               
ANM20    DS    0H                 MEDIA/SOURCE SECTION FOUND                    
         MVC   DUB(2),DAMHLDE      SET LENGTH OF DATA ELEMENT                   
         LR    R7,R6               POINT TO DATA ELEMENTS                       
         AHI   R7,DAMHDRL                                                       
         LAM   AR7,AR7,ALET                                                     
         USING DAMDTAD,R7                                                       
                                                                                
         CLC   0(5,R2),=C'ANMKT'                                                
         BE    ANM100                                                           
         CLC   0(5,R2),=C'NAMKT'                                                
         BE    ANM200                                                           
         CLC   0(6,R2),=C'NAXMKT'  NUMERIC-TO-ALPHA (MATCH BOOKTYPE)            
         BE    ANM200                                                           
         DC    H'0'                                                             
*                                                                               
** TRANSLATE ALPHA TO NUMERIC MARKET **                                         
*                                                                               
ANM100   XC    0(2,R3),0(R3)       1ST TWO BYTES USED FOR BINARY MKT#           
         MVC   2(4,R3),=CL16' '     NEXT 4 BYTES FOR EBCDIC MKT CODE            
         OC    DBSELALF,DBSELALF   IF NO ALPHA MARKET                           
         BZ    ANMXSM                                                           
         CLC   DBSELALF,=CL16' '                                                
         BE    ANMXSM               EXIT NOW                                    
*                                                                               
ANM110   CLC   DAMDAMKT,DBSELALF                                                
         BL    ANM117                                                           
         BE    ANM115                                                           
         BH    ANMXSM                                                           
                                                                                
ANM115   CLI   DAMDBTYP,X'FF'      IF DEFAULT BOOKTYPE REACHED,                 
         BE    ANM120               USE IT                                      
         CLC   DAMDBTYP,DBBTYPE    ELSE, SEE IF WE COULD MATCH ON BTYPE         
         BE    ANM120                                                           
                                                                                
ANM117   AH    R7,DUB              BUMP TO NEXT ENTRY                           
         CLM   R7,15,DAMHAET       COMPARE AGAINST EOTABLE ADDRESS              
         BL    ANM110               KEEP LOOKING IF NOT EOTABLE                 
         B     ANMXSM                                                           
                                                                                
ANM120   XR    R0,R0               ALPHAMKT ENTRY FOUND                         
         ICM   R0,3,DAMDNMKT                                                    
         STCM  R0,3,0(R3)          PASS BACK BINARY                             
         SAC   0                                                                
         EDIT  (R0),(4,2(R3)),FILL=0   AND EBCDIC MARKET NUMBER                 
         B     ANMXSM                                                           
*                                                                               
** TRANSLATE NUMERIC TO ALPHA MARKET **                                         
*                                                                               
ANM200   MVC   0(3,R3),=CL16' '    INITIALIZE OUTPUT AREA                       
         OC    DBSELRMK,DBSELRMK   IF NO RTG SVC MARKET,                        
         BZ    *+14                                                             
         MVC   WORK+1(2),DBSELRMK                                               
         B     ANM210                                                           
                                                                                
         OC    DBSELMK,DBSELMK      AND NO SPILL MARKET,                        
         BZ    *+14                                                             
         MVC   WORK+1(2),DBSELMK                                                
         B     ANM210                                                           
                                                                                
         LA    R0,DMFRSTEL-DMKEY    TRY MARKET NAME ELEMENT                     
         CLI   DBRECTYP,DBRECMK      OF MARKET RECORD                           
         BE    ANM205                                                           
         B     ANMXSM               EXIT NOW IF NOTHING WORKS                   
                                                                                
ANM205   ICM   R1,15,DBAREC                                                     
         BZ    ANMXSM                                                           
         AR    R1,R0               R1-->MARKET NAME ELEMENT                     
         USING DMELEM,R1                                                        
         CLI   DMELEM,DMECODEQ                                                  
         BNE   ANMXSM                                                           
         MVC   WORK+1(2),DMMNO     GET MARKET NUMBER INTO WORK+1                
         B     ANM210                                                           
         DROP  R1                                                               
*                                                                               
ANM210   DS    0H                  WORK+1(2)=MKT#                               
         CLC   DAMDNMKT,WORK+1     MUST MATCH ON MARKET NUMBER                  
         BNE   ANM220                                                           
*                                                                               
         CLC   0(6,R2),=C'NAXMKT'  EXACT BOOKTYPE MATCH REQUIRED?               
         BE    *+12                YES                                          
         CLI   DAMDBTYP,X'FF'      IF DEFAULT BOOKTYPE REACHED,                 
         BE    ANM211               USE IT                                      
         CLC   DAMDBTYP,DBBTYPE    ELSE, SEE IF WE COULD MATCH ON BTYPE         
         BNE   ANM220                                                           
                                                                                
*                                  FOUND ALPHAMKT ENTRY                         
ANM211   MVC   0(3,R3),DAMDAMKT    PASS BACK ALPHAMKT                           
         B     ANMXSM                                                           
*                                                                               
ANM220   AH    R7,DUB                                                           
         CLM   R7,15,DAMHAET       COMPARE AGAINST EOTABLE ADDRESS              
         BL    ANM210               KEEP LOOKING IF NOT EOTABLE                 
         B     ANMXSM                                                           
*                                                                               
** EXIT TRANSLATION ROUTINE **                                                  
*                                                                               
ANMXSM   SAC   0                   RESTORE 24-BIT MODE BEFORE EXIT              
         LAM   AR6,AR7,=2F'0'                                                   
         SAM24                                                                  
*                                                                               
ANMXX    B     XIT                                                              
         DROP  R6,R7                                                            
*                                                                               
         SPACE 3                                                                
DEMTABCL CSECT                                                                  
*                                                                               
TABLIST  DS    0X                                                               
         DC    X'E3',3X'00'        ALPHAMKT TABLE (IN DATASPACE)                
         DC    X'FF'                                                            
TABLISTQ EQU   *-TABLIST                                                        
         EJECT                                                                  
DEFINE   RSECT                                                                  
*                                                                               
*---------------------------------------------------------------------*         
*            NAD MONTH TABLE - RTNS: A(TABLE) & NETWEEKS                        
* PICKS UP ADDRESS OF NAD MONTHS TABLE AND RETURNS IN 1ST 4 BYTES.              
* IF VALID MONTH BOOK IN DBSELBK, GETS THE NETWEEK VALUES FOR ALL WKS           
* THAT MAKE UP THE MONTH.  RETURNS THEM IN 2-BYTE FIELDS FOLLOWING              
* THE ADDR OF THE TABLE.                                                        
*---------------------------------------------------------------------*         
NDMON    DS    0H                                                               
         XC    0(14,R3),0(R3)      CLEAR OUTPUT AREA                            
         XC    WORK,WORK                                                        
*                                                                               
         L     R7,DBCOMFCS                                                      
         USING COMFACSD,R7                                                      
         GOTO1 CDEMTABS,DMCB,MTHDATES   GET A(MONTH/DATES TABLE)                
         ICM   R6,15,0(R1)         R6=A(MONTH/DATES TABLE)                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,0(R3)                                                         
         L     R5,4(R1)            R5 HAS LENGTH OF TABLE ENTRY                 
*                                                                               
         USING MTHDATD,R6                                                       
NDM10    CLI   MDNLSMN,12                                                       
         BH    NDMX                NOT FOUND                                    
         CLC   DBSELBK,MDNLBOOK    MATCH BOOK TO TABLE                          
         BE    NDM20                                                            
         AR    R6,R5               NEXT ENTRY IN THE TABLE                      
         B     NDM10                                                            
*                                  START WEEK OF MONTH                          
NDM20    OC    CCALLOV,CCALLOV                                                  
         BZ    NDMX                CAN'T GET ADDRESS OF NETWEEK                 
         LA    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D9000A17'                                           
         GOTO1 CCALLOV,(R1),0         GET A(DEGET01) FROM CALLOV                
         MVC   ANETWEEK,DMCB       SAVE AWAY FOR LATER                          
         OC    ANETWEEK,ANETWEEK                                                
         BZ    NDMX                                                             
*                                                                               
*                                  START WEEK OF MONTH                          
         GOTO1 CDATCON,DMCB,(3,MDSDATE),(0,DUB),0   CONV TO EBCIDIC             
         GOTO1 ANETWEEK,DMCB,DUB,CGETDAY,CADDAY        FOR NETWEEK              
         MVC   WORK(1),4(R1)       YEAR                                         
         MVC   WORK+1(1),8(R1)     START WEEK WEEK                              
         GOTO1 CDATCON,DMCB,(3,MDEDATE),(0,DUB),0                               
         GOTO1 ANETWEEK,DMCB,DUB,CGETDAY,CADDAY                                 
         DROP  R6,R7                                                            
         ZIC   R0,8(R1)            END WEEK                                     
         ZIC   RE,WORK+1           START WEEK                                   
         SR    R0,RE               # WKS-1=START WK-END WK                      
         LA    R7,WORK+6           END WK = 4TH WEEK SLOT?                      
         CHI   R0,4                IF 4 WK MTH R0=3, 5 WK MON R0=4              
         BL    *+8                                                              
         LA    R7,WORK+8           SLOT END WK IN 5TH WK SLOT                   
         MVC   0(1,R7),4(R1)                                                    
         MVC   1(1,R7),8(R1)       SLOT LAST WK IN CORRECT SLOT                 
         ZIC   R1,WORK+1           FILL IN BETWEEN SLOTS BY BUMPING WK          
         LA    R7,WORK+2           START WITH 2ND WK BUCKET                     
*                                                                               
NDM30    LA    R1,1(R1)            BUMP WEEK                                    
         MVC   0(1,R7),WORK        SET YEAR                                     
         STC   R1,1(R7)            SET WEEK                                     
         LA    R7,2(R7)            NEXT WK SLOT                                 
         BCT   R0,NDM30                                                         
*                                                                               
         MVC   4(10,R3),WORK       MOVE WKS INTO OUTPUT FIELD                   
                                                                                
NDMX     B     XIT                                                              
         EJECT                                                                  
*-------------------- NUMBER OF DAYS A PROGRAM RAN -------------------*         
                                                                                
NDAYS    DS    0H                                                               
         MVI   GOSUBN,NDAYS#                                                    
         GOTO1 AGOSUB                                                           
                                                                                
*                                                                               
         B     XIT                                                              
         TITLE '-   DEMO RECORD DATA EXTRACTOR (MISCELLANEOUS)'                 
***********************************************************************         
*===================== SUBROUTINE POOL INTERFACE =====================*         
                                                                                
GOSUB    NTR1  BASE=MYBASE1,LABEL=N                                             
         L     R8,MYBASE2                                                       
                                                                                
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'+GO'                                                  
         MVC   3(1,RE),GOSUBN                                                   
         SR    RE,RE                AND CLEAR  RE  JUST TO BE SAFE              
                                                                                
*                                                                               
         L     RE,=A(SUBRTNTB-DEFINE)                                           
         LA    RE,DEFINE(RE)                                                    
                                                                                
GOSUB22  DS    0H                                                               
         CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   GOSUBN,0(RE)                                                     
         BNH   *+12                                                             
         LA    RE,L'SUBRTNTB(RE)                                                
         B     GOSUB22                                                          
*                                                                               
         ZICM  RF,1(RE),(3)                                                     
         A     RF,MYBASE1                                                       
                                                                                
*                                                                               
GOSUBGO  DS    0H                                                               
         GOTO1 (RF),(RC)                                                        
*                                                                               
         XIT1                                                                   
***********************************************************************         
         DROP  RB,R8                                                            
         EJECT                                                                  
*--------------- LITERAL POOL ----------------------------------------*         
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* --------------------------------------------------------------------          
                                                                                
*              LIVE INDICATOR                                                   
NLIV     NTR1  BASE=*,LABEL=*                                                   
         MVI   0(R3),0                                                          
         MVC   1(L'VWDSCP,R3),=CL30' '                                          
*                                                                               
         L     RE,DBAREC                                                        
         USING PRKEY,RE                                                         
         CLI   PRCODE,PRCODEQU     C'P' TIME PERIOD DATA                        
         BE    NLIV05                                                           
         CLI   PRCODE,PMCODEQU     C'Q' PROGRAM DATA                            
         BE    NLIV05                                                           
         CLI   PRCODE,MXMCODEQ     C'M' MINUTE BY MINUTE DATA                   
         BNE   NLIVX                                                            
         DROP  RE                                                               
*                                                                               
NLIV05   ICM   RF,15,DBCOMFCS                                                   
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,VWDESCTB  GET A(VIEWING TYPES DESCRIPTION)             
         ICM   R5,15,DMCB          A(TABLE) RETURNED IN P1                      
         ICM   R0,15,DMCB+4        l(TABLE) RETURNED IN P2                      
         BNZ   *+6                 TEMP                                         
         DC    H'0'                BAD TABLEID PASSED                           
         USING VWDESCD,R5                                                       
         MVC   0(1,R3),VWDSRC      LIVE IS DEFAULT (FIRST ENTRY)                
         MVC   1(L'VWDSCP,R3),VWDSCP                                            
*                                                                               
         ICM   R4,15,DBSPANAD      EXTRACT VIEWING TYPE FROM SPAN AREA          
         BZ    NLIV20                                                           
         CLC   0(3,R4),=C'CB2'     CABLE SPAN AREA FOR CABLE NAD & MVGO         
         BE    NLIV10                                                           
         CLC   0(3,R4),=C'CAB'     CABLE SPAN AREA FOR REG CABLE                
         BE    NLIV15                                                           
         BNE   NLIV20                                                           
NLIV10   CLI   135(R4),X'FF'       USE SPAN AREA FOR CABLE NAD AND MVGO         
         BE    NLIV50              INVALID VIEWING TYPE                         
         CLI   0(R5),X'FF'                                                      
         BE    NLIV50              INVALID VIEWING TYPE                         
         CLC   VWDSRC,135(R4)      COMPARE ON VIEWING TYPE CODE                 
         BE    NLIV40              EXTRACT VIEWING TYPE INFO                    
         AR    R5,R0                                                            
         B     NLIV10                                                           
*                                                                               
NLIV15   CLI   0(R5),X'FF'         FOR REG CABLE, USE VIEWTYP FORM KEY          
         BE    NLIV50              INVALID VIEWING TYPE                         
         L     RE,DBAREC                                                        
         CLC   VWDSRC,2(RE)                                                     
         BE    NLIV40                                                           
         AR    R5,R0                                                            
         B     NLIV15                                                           
*                                                                               
NLIV20   L     R6,DBAREC           EXTRACT VIEWING TYPE FROM ELEMENT            
         MVI   ELCODE,DTCODEQ                                                   
         MVC   DATADISP,=AL2(PRFRSTEL-PRKEY) START OF RECD,NOT DBAQUART         
         BRAS  RE,GETEL                                                         
         BNE   NLIVX                                                            
         USING DTELEM,R6                                                        
NLIV30   CLI   0(R5),X'FF'                                                      
         BE    NLIV50                                                           
         ZIC   R1,DTLEN                                                         
         SHI   R1,DTLENQ                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DTTYPE(0),VWDSCP                                                 
         BE    NLIV40                                                           
         AR    R5,R0                                                            
         B     NLIV30                                                           
*                                                                               
NLIV40   MVC   0(1,R3),VWDSRC           SOURCE/VVIEWING INDICATOR               
         MVC   1(L'VWDSCP,R3),VWDSCP    VIEWING DESCRIPTION                     
         B     NLIVX                                                            
*                                                                               
NLIV50   MVI   0(R3),0                  INVALID DATA IN ELEMENT                 
         MVC   1(L'VWDSCP,R3),=CL30' '                                          
         B     NLIVX                                                            
*                                                                               
NLIVX    J     XIT2                                                             
         DROP  R5,R6                                                            
         LTORG                                                                  
                                                                                
*              PREMIERE INDICATOR                                               
                                                                                
PREM     NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,X'10'        FOR NETWORK PROGRAMS                         
         MVI   0(R3),0                                                          
         L     RE,DBAREC                                                        
         CLI   0(RE),MXMCODEQ      ONLY FOR PROGRAM RECORDS                     
         BE    *+12                                                             
         CLI   0(RE),PMCODEQU                                                   
         BNE   PREMX                                                            
         BRAS  RE,GETEL                                                         
         BNE   PREMX                                                            
         USING PHTELEM,R6                                                       
         MVC   0(1,R3),PHTPREM                                                  
PREMX    J     XIT2                                                             
                                                                                
*              EPISODE TITLES                                                   
EPIS     DS    0D                                                               
EPIS1    NTR1  BASE=*,LABEL=*                                                   
         LA    RF,15               DEFAULT OUTPUT LEN = 16                      
         CLC   0(6,R2),=C'EPISDD'                                               
         BE    *+10                                                             
         CLC   0(6,R2),=C'EPIS25'                                               
         BNE   *+8                                                              
         LA    RF,24               OPTION FOR 25 FOR SOME FILES                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=CL25' '                                                 
         LA    R0,1(RF)                                                         
         CLC   =C'ITN',DBSELSTA                                                 
         BNE   *+14                                                             
         CLC   0(6,R2),=C'EPISDD'  ONLY DDS DISPLAY ALLOWED FOR ITN             
         JNE   XIT2                                                             
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    EPIS2                                                            
         CLC   MYDBFILE,=C'RLD'                                                 
         BE    EPIS2A                                                           
         J     XIT2                                                             
         SPACE 1                                                                
EPIS2    ICM   RE,15,DBSPANAD      CHECK FOR SPANNED RECORDS                    
         BNZ   EPIS4                                                            
*                                                                               
EPIS2A   CLC   =C'RLD',MYDBFILE    CABLE RLD NEEDS SPECIAL HANDLING             
         BNE   EPIS2B                                                           
         L     RE,DBAREC                                                        
         USING MXMKEY,RE                                                        
         CLI   MXMMEDIA,C'C'       CABLE                                        
         BE    EPIS2C                                                           
         DROP  RE                                                               
*                                                                               
EPIS2B   MVI   ELCODE,X'24'        PAVFILE                                      
         BRAS  RE,GETEL                                                         
         JNE   XIT2                                                             
         USING PPTELEM,R6                                                       
         LA    R9,PPTTITLE                                                      
         ZIC   R1,PPTELN                                                        
         SHI   R1,3                                                             
         CR    R1,R0               TEST FOR LONG EPISODE NAMES                  
         BH    EPIS3                                                            
         EX    R1,*+8                                                           
         J     XIT2                                                             
         MVC   0(0,R3),PPTTITLE                                                 
         DROP  R6                                                               
                                                                                
EPIS2C   MVC   WORKL(26),=CL26' '  SPECIAL HANDLING FPR CABLE RLD               
         MVI   ELCODE,X'24'        EPISODE ELEMENT                              
         BRAS  RE,GETEL                                                         
         BNE   EPIS2D                                                           
         USING PPTELEM,R6                                                       
         LA    R9,PPTTITLE                                                      
         ZIC   R1,PPTELN                                                        
         SHI   R1,3                                                             
         CHI   R1,24                                                            
         BNH   *+8                                                              
         LHI   R1,24               MAX 25-CHAR LONG TO MATCH NTI CABLE          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORKL(0),PPTTITLE                                                
         DROP  R6                                                               
         LA    R9,WORKL                                                         
         LA    R1,25                                                            
         B     EPIS3                                                            
*                                                                               
EPIS2D   L     R6,DBAREC           EPISODE ELEMENT NOT FOUND.                   
         MVI   ELCODE,PPNCODEQ      USE PROGRAM NAME.                           
         BRAS  RE,GETEL                                                         
         JNE   XIT2                                                             
         USING PPNELEM,R6                                                       
         ZIC   R1,PPNELN                                                        
         SHI   R1,3                EXCLUDE ELEMENT CODE AND LENGTH              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORKL(0),PPNNME                                                  
         LA    R9,WORKL                                                         
         LA    R1,25                                                            
         B     EPIS3                                                            
         DROP  R6                                                               
*                                                                               
EPIS3    LA    RF,1(R1)            RESTORE PROGRAM NAME LENGTH                  
         GOTO1 =V(MININAM),DMCB,((RF),(R9)),((R0),(R3)),               X        
               =C'PNABBR',RR=RELO                                               
         J     XIT2                                                             
*                                                                               
EPIS4    CLC   0(3,RE),=C'CB2'     CABLE NAD AND REG CABLE                      
         BE    *+10                                                             
         CLC   0(3,RE),=C'CAB'     IF NOT CABLE - FORGET IT                     
         BNE   EPIS2A               JUST DO NORMAL STUFF                        
         CLI   3(RE),2                                                          
         JNE   XIT2                                                             
         LA    R1,25                                                            
         LA    R9,64(RE)                                                        
         B     EPIS3                                                            
XIT2     XIT1                                                                   
         EJECT                                                                  
* GET FEED                                                                      
FEED     DS    0D                                                               
FEED1    NTR1  BASE=*,LABEL=*                                                   
         L     RE,DBAREC                                                        
         USING PMKEY,RE                                                         
         MVI   0(R3),C' '                                                       
         CLC   =C'NTI',MYDBFILE    NTI FILE                                     
         BE    *+14                                                             
         CLC   =C'RLD',MYDBFILE    AND RLD                                      
         BNE   FEEDXIT                                                          
*                                                                               
         CLI   PMCODE,PMCODEQU     C'Q' PROGRAM DATA                            
         BE    *+12                                                             
         CLI   PMCODE,MXMCODEQ     C'M' MINUTE BY MINUTE DATA                   
         BNE   FEEDXIT                                                          
*                                                                               
         CLI   PMMEDIA,C'N'        NETWORK                                      
         BE    FEED2               OR                                           
         CLI   PMMEDIA,C'n'        NETWORK COMMERCIAL AVERAGE                   
         BE    FEED2               OR                                           
         CLI   PMMEDIA,C'C'        CABLE                                        
         BE    FEED2               OR                                           
         CLI   PMMEDIA,C'c'        CABLE COMMERCIAL AVERAGE                     
         BNE   FEEDXIT                                                          
         DROP  RE                                                               
FEED2    MVI   ELCODE,X'22'                                                     
         L     R6,DBAQUART                                                      
*FOR CABLE NAD, POINT TO NAD INFO IN SPANAD                                     
         ICM   RE,15,DBSPANAD                                                   
         BZ    FEED5                                                            
         AHI   RE,320              DISPLACEMENT TO CBL NAD INFO LABEL           
         CLC   0(4,RE),=C'NDEF'                                                 
         BNE   FEED5                                                            
         LA    RE,4(RE)            POINT TO CABLE NAD INFO                      
         LR    R6,RE               (KEY + DESCRIPTIVE ELEMENTS)                 
         AH    R6,DATADISP                                                      
*                                                                               
FEED5    BRAS  RE,FIRSTEL                                                       
         BNE   FEEDXIT                                                          
         USING NTELEM,R6                                                        
         MVC   0(1,R3),NTFEED                                                   
         DROP  R6                                                               
FEEDXIT  XIT1                                                                   
                                                                                
                                                                                
* GET LIVE INDICATOR                                                            
LIVE     DS    0D                                                               
LIVE1    NTR1  BASE=*,LABEL=*                                                   
         L     RE,DBAREC                                                        
         USING PMKEY,RE                                                         
         MVI   0(R3),C' '                                                       
         CLC   =C'NTI',MYDBFILE    NTI FILES                                    
         BE    LIVE3                                                            
         CLC   =C'RLD',MYDBFILE    RLD FILES                                    
         BE    LIVE3                                                            
         CLC   =C'NAD',MYDBFILE    NAD FILES                                    
         BNE   LIVEXIT                                                          
*                                                                               
LIVE3    CLI   0(RE),PMCODEQU      PROGRAM RECORD                               
         BE    *+12                                                             
         CLI   0(RE),MXMCODEQ      MXM PROGRAM RECORD                           
         BNE   LIVEXIT                                                          
         DROP  RE                                                               
         MVI   ELCODE,X'22'                                                     
         L     R6,DBAQUART                                                      
*FOR CABLE NAD, POINT TO NAD INFO IN SPANAD                                     
         ICM   RE,15,DBSPANAD                                                   
         BZ    LIVE5                                                            
         AHI   RE,320              DISPLACEMENT TO CBL NAD INFO LABEL           
         CLC   0(4,RE),=C'NDEF'                                                 
         BNE   LIVE5                                                            
         LA    RE,4(RE)            POINT TO CABLE NAD INFO                      
         LR    R6,RE               (KEY + DESCRIPTIVE ELEMENTS)                 
         AH    R6,DATADISP                                                      
*                                                                               
LIVE5    BRAS  RE,FIRSTEL                                                       
         BNE   LIVEXIT                                                          
         USING NTELEM,R6                                                        
         MVC   0(1,R3),NTLIVE                                                   
         DROP  R6                                                               
LIVEXIT  XIT1                                                                   
                                                                                
                                                                                
MPCOST   DS    0D                                                               
MPCOST1  NTR1  BASE=*,LABEL=*                                                   
*        GET MONITOR PLUS COST                                                  
         XC    0(4,R3),0(R3)                                                    
         L     RE,DBAREC                                                        
         USING PMKEY,RE                                                         
         CLC   =C'NTI',MYDBFILE    NTI FILE                                     
         BNE   MPCXIT                                                           
         CLI   PMCODE,PMCODEQU     C'Q' PROGRAM DATA                            
         BNE   MPCXIT                                                           
         CLI   PMMEDIA,C'N'        NETWORK                                      
         BE    MPCOST5             OR                                           
         CLI   PMMEDIA,C'n'        NETWORK COMMERCIAL AVERAGE                   
         BE    MPCOST5             OR                                           
         CLI   PMMEDIA,C'C'        CABLE                                        
         BE    MPCOST5             OR                                           
         CLI   PMMEDIA,C'c'        CABLE COMMERCIAL AVERAGE                     
         BNE   MPCXIT                                                           
MPCOST5  CLI   PMBTYP,C'C'         AND BOOK TYPE C                              
         BNE   MPCXIT                                                           
         DROP  RE                                                               
         MVI   ELCODE,X'41'                                                     
         L     R6,DBAQUART                                                      
         BRAS  RE,FIRSTEL                                                       
         BNE   MPCXIT                                                           
         ICM   RE,15,3(R6)                                                      
         MHI   RE,100              CABLE IN HUNDREDS                            
         L     RF,DBAREC                                                        
         USING PMKEY,RF                                                         
         CLI   PMSTAT+4,C'H'       HISPANIC IN HUNDEREDS                        
         BE    *+8                                                              
         CLI   PMSTAT+4,C'C'                                                    
         BE    *+8                                                              
         MHI   RE,10               NET/SYND IN 1000                             
         ST    RE,0(R3)                                                         
         DROP  RF                                                               
MPCXIT   XIT1                                                                   
         SPACE 2                                                                
*                                                                               
DAYNTR   DS    0D                                                               
DAYNTR1  NTR1  BASE=*,LABEL=*                                                   
         XC    0(2,R3),0(R3)                                                    
         MVC   2(3,R3),=CL16' '                                                 
         CLC   MYDBFILE,=C'NAD'                                                 
         BE    DAY1                                                             
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    DAY1                                                             
         CLC   MYDBFILE,=C'RLD'                                                 
         BE    DAY1                                                             
         CLC   MYDBFILE,=C'PAV'                                                 
         BE    DAY2                                                             
         CLC   =C'TP',MYDBFILE                                                  
         BE    DAY6                                                             
         CLC   =C'CTP',MYDBFILE                                                 
         BE    DAY6                                                             
         CLC   MYDBFILE,=C'INV'                                                 
         BE    DAY4                                                             
         CLC   MYDBFILE,=C'IUN'                                                 
         BE    DAYIUN                                                           
         CLC   MYDBFILE,=C'RDP'    TEST RADIO DAYPART                           
         BE    DAY6                                                             
         CLC   MYDBFILE,=C'MPA'                                                 
         BE    DAY7                                                             
         B     DAYXIT                                                           
         SPACE 1                                                                
DAYIUN   ICM   RE,15,DBEXTEND                                                   
         BZ    DAYXIT                                                           
DAYIUN2  CLC   0(4,RE),=C'RINV'                                                 
         BE    DAYIUN4                                                          
         ICM   RE,15,4(RE)                                                      
         BZ    DAYXIT                                                           
         B     DAYIUN2                                                          
         USING DBXINVWK,RE                                                      
DAYIUN4  MVC   1(1,R3),DBXIDAY                                                  
         LARL  R1,DAYTAB                                                        
DAYIUN6  CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    DAYXIT                                                           
         CLC   0(1,R1),1(R3)                                                    
         BE    *+12                                                             
         LA    R1,L'DAYTAB(R1)                                                  
         B     DAYIUN6                                                          
         MVC   0(1,R3),0(R1)                                                    
         MVC   2(3,R3),1(R1)                                                    
         B     DAYXIT                                                           
         DROP  RE                                                               
         SPACE 1                                                                
DAY1     DS    0H                                                               
         USING PMKEY,R6                                                         
         CLI   PMCODE,MXMCODEQ     C'M' MINUTE BY MINUTE DATA                   
         BE    DAY1A                                                            
         CLI   PMCODE,PMCODEQU     C'Q' PROGRAM DATA                            
         BNE   DAY01                                                            
         CLI   PMMEDIA,C'C'        CABLE                                        
         BE    DAY1A                                                            
         CLI   PMMEDIA,C'c'        CABLE COMMERCIAL AVERAGE                     
         BE    DAY1A                                                            
         DROP  R6                                                               
DAY01    CLI   DBRECTYP,DBRECNTI   TEST FOR NETWK PROG RECD                     
         BNE   DAY2                                                             
         ZIC   R1,DBACTDAY         PICK UP DAY SET IN DBLOCK                    
         B     DAY2A                                                            
         SPACE 1                                                                
DAY1A    DS    0H                  GET DAY FROM NTELEM FOR CBL PRGMS            
         MVI   ELCODE,X'22'        RUN TIME ELEMENT                             
         BRAS  RE,GETEL                                                         
         USING NTELEM,R6                                                        
         MVC   1(1,R3),NTDAY       GET DAY CODE                                 
         LARL  R1,DAYTAB                                                        
DAY1B    CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    DAYXIT                                                           
         CLC   0(1,R1),NTDAY                                                    
         BE    *+12                                                             
         LA    R1,L'DAYTAB(R1)                                                  
         B     DAY1B                                                            
         MVC   0(1,R3),0(R1)                                                    
         MVC   2(3,R3),1(R1)                                                    
         B     DAYXIT                                                           
         DROP  R6                                                               
         SPACE 1                                                                
         USING PRKEY,R6                                                         
DAY2     ZIC   R1,PRDW                                                          
         SPACE 1                                                                
DAY2A    SRL   R1,4                                                             
         SPACE 1                                                                
DAY3     STC   R1,1(R3)                                                         
         MHI   R1,L'DAYTAB                                                      
         LARL  RF,DAYTAB                                                        
         AR    R1,RF                                                            
         MVC   0(1,R3),0(R1)                                                    
         MVC   2(3,R3),1(R1)                                                    
                                                                                
*                                  SPECIFY # OF DAYS FOR PAV                    
         USING PHELEM,R7                                                        
         CLC   MYDBFILE,=C'PAV'     ONLY FOR PAV FILE                           
         BNE   DAY3PVX                                                          
         CLI   1(R3),8              ONLY IF KEY DAY IS 8                        
         BL    DAY3PVX                                                          
         CLI   1(R3),9               OR 9                                       
         BH    DAY3PVX                                                          
                                                                                
         CLI   PHCODE,PHCODEQ       LOOK IN DAY/QH ELEMENT                      
         BNE   DAY3PVX                                                          
         CLI   PHREVID,X'FF'        MAKE SURE REVISION ID                       
         BNE   DAY3PVX                                                          
         CLI   PHREV,2               IS THE RIGHT ONE                           
         BL    DAY3PVX                                                          
         CLC   PHNDAYS,4(R1)        IF ACTUAL # DAYS DIFF FROM TABLE,           
         BE    DAY3PVX                                                          
         MVC   0(1,R3),PHNDAYS       SET SPECIAL INTERNAL DAY CODE              
         OI    0(R3),X'90'                                                      
         MVC   2(2,R3),=C'AV'        USE A MORE DESCRIPTIVE DAY CODE            
         MVC   4(1,R3),PHNDAYS                                                  
         OI    4(R3),X'F0'                                                      
DAY3PVX  EQU   *                                                                
         DROP  R7                                                               
                                                                                
         B     DAYXIT                                                           
         SPACE 1                                                                
         USING RINVKEY,R6                                                       
DAY4     ZIC   R1,RINVKDAY                                                      
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         B     DAY3                                                             
         SPACE 1                                                                
DAY6     CLC   =C'RD',0(R6)        DPT FILE?                                    
         BNE   DAY6A                                                            
         CLI   2(R6),C'P'          NOT RDP THOUGH                               
         BE    DAY6A                                                            
         USING QHELEM,R6                                                        
         MVI   ELCODE,QHCODEQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   DAY6A                                                            
         TM    QHDAY,X'0F'                                                      
         BZ    DAY6B                                                            
         OI    QHDAY,X'80'                                                      
         B     DAY6B                                                            
DAY6A    LR    R6,R7                                                            
DAY6B    ZIC   R1,QHDAY                                                         
         SLL   R1,25                                                            
         SRL   R1,29                                                            
         CLI   QHDAY,X'95'         M-F                                          
         BNE   DAY6C                                                            
         SR    R1,R1                                                            
         B     DAY3                                                             
DAY6C    CLI   QHDAY,X'97'         M-S                                          
         BNE   DAY6X                                                            
         LA    R1,8                                                             
DAY6X    B     DAY3                                                             
         USING MDAYELEM,R6                                                      
DAY7     MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         MVC   1(1,R3),MDTELDAY    GET DAY CODE                                 
         LARL  R1,DAYTAB                                                        
DAY8     CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    DAYXIT                                                           
         CLC   0(1,R1),MDTELDAY                                                 
         BE    DAY9                                                             
         LA    R1,L'DAYTAB(R1)                                                  
         B     DAY8                                                             
*                                                                               
DAY9     MVC   0(1,R3),0(R1)                                                    
         MVC   2(3,R3),1(R1)                                                    
         B     DAYXIT                                                           
*                                                                               
DAYXIT   XIT1                                                                   
         SPACE 2                                                                
*                                                                               
COMMST   DS    0D                                                               
COMMST1  NTR1  BASE=*,LABEL=*                                                   
         MVC   0(3,R3),=C'N/A'                                                  
         CLC   =C'NTI',MYDBFILE    NTI FILE                                     
         BNE   COMMSTX                                                          
         L     RE,DBAREC                                                        
         USING PMKEY,RE                                                         
         CLI   PMCODE,PMCODEQU     C'Q' PROGRAM DATA                            
         BNE   COMMSTX                                                          
         CLI   PMMEDIA,C'C'        CABLE                                        
         BE    *+12                                                             
         CLI   PMMEDIA,C'c'        CABLE COMMERCIAL AVERAGE                     
         BNE   COMMSTX                                                          
         DROP  RE                                                               
         MVI   ELCODE,X'22'                                                     
         L     R6,DBAQUART                                                      
*FOR CABLE NAD, POINT TO NAD INFO IN SPANAD                                     
         ICM   RE,15,DBSPANAD                                                   
         BZ    COMMST5                                                          
         AHI   RE,320              DISPLACEMENT TO CBL NAD INFO LABEL           
         CLC   0(4,RE),=C'NDEF'                                                 
         BNE   COMMST5                                                          
         LA    RE,4(RE)            POINT TO CABLE NAD INFO                      
         LR    R6,RE               (KEY + DESCRIPTIVE ELEMENTS)                 
         AH    R6,DATADISP                                                      
*                                                                               
COMMST5  BRAS  RE,FIRSTEL                                                       
         BNE   COMMSTX                                                          
         USING NTELEM,R6                                                        
         MVC   0(3,R3),=C'   '     BLANK PADDED                                 
         MVC   0(1,R3),NTCMCL                                                   
         DROP  R6                                                               
COMMSTX  XIT1                                                                   
         LTORG                                                                  
*                                                                               
MBINF    DS    0D                                                               
MBINF1   NTR1  BASE=*,LABEL=*      MARKET BREAK INFO                            
         MVI   ELCODE,MKBCODEQ                                                  
MBINF2   BRAS  RE,GETEL            GET X'29' ELEM                               
         BE    *+14                                                             
         XC    0(2,R3),0(R3)                                                    
         B     MBINFX                                                           
         USING MKBELEM,R6          REQUESTED MKT BREAK?                         
MBINF3   CLC   MKBBRK,0(R3)                                                     
         BE    MBINF5                                                           
MBINF4   BRAS  RE,NEXTEL           GET THE NEXT X'29'                           
         BE    *+14                                                             
         XC    0(2,R3),0(R3)                                                    
         B     MBINFX                                                           
         CLI   0(R6),MKBCODEQ                                                   
         BE    MBINF3                                                           
         BNE   MBINF4                                                           
MBINF5   MVC   0(2,R3),MKBSEC      MARKET BREAK SPCIFIC COMMERCIAL DUR          
         DROP  R6                                                               
MBINFX   XIT1                                                                   
         LTORG                                                                  
*                                                                               
CTYINFO  DS    0D                                                               
CTYINFO1 NTR1  BASE=*,LABEL=*                                                   
         MVC   0(2,R3),=C'NA'                                                   
         CLC   =C'CTP',MYDBFILE    NTI FILE                                     
         BNE   CTYINFOX                                                         
         L     R6,DBAREC                                                        
         AH    R6,DATADISP                                                      
CTYNXT   CLI   0(R6),0             FIND THE COUNTY NAME ELEMENT                 
         BE    CTYINFOX                                                         
         CLI   0(R6),7                                                          
         BE    CTYELOK                                                          
         BH    CTYINFOX                                                         
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         J     CTYNXT                                                           
CTYELOK  CLI   3(R2),C'N'                                                       
         BNE   *+14                                                             
         MVC   0(16,R3),4(R6)      COUNTY NAME                                  
         B     CTYSTA                                                           
         CLI   3(R2),C'#'                                                       
         BNE   *+10                                                             
         MVC   0(2,R3),2(R6)       COUNTY NUMBER                                
         B     CTYINFOX                                                         
*                                                                               
CTYSTA   L     R6,DBAREC           ADD STATE TO COUNTY NAME                     
         USING DRKEY,R6                                                         
         LARL  RE,STACODE                                                       
CTYSTA5  CLC   =X'FFFF',0(RE)                                                   
         BE    CTYINFOX                                                         
         CLC   DRBTYP,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,28(RE)                                                        
         B     CTYSTA5                                                          
         MVC   16(4,R3),=C'(  )'                                                
         MVC   17(2,R3),1(RE)      STATE CODE                                   
*                                                                               
CTYINFOX XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
DMAINFO  DS    0D                                                               
DMAINFO1 NTR1  BASE=*,LABEL=*                                                   
         MVC   0(2,R3),=C'NA'                                                   
         CLC   =C'CTP',MYDBFILE    NTI FILE                                     
         BNE   DMAINFOX                                                         
         L     R6,DBAREC                                                        
         AH    R6,DATADISP                                                      
DMANXT   CLI   0(R6),0             FIND THE DMA NAME ELEMENT                    
         BE    DMAINFOX                                                         
         CLI   0(R6),8                                                          
         BE    DMAELOK                                                          
         BH    DMAINFOX                                                         
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         J     DMANXT                                                           
DMAELOK  CLI   3(R2),C'N'                                                       
         BNE   *+10                                                             
         MVC   0(26,R3),4(R6)      DMA NAME                                     
         CLI   3(R2),C'#'                                                       
         BNE   *+10                                                             
         MVC   0(2,R3),2(R6)       DMA NUMBER                                   
         CLI   3(R2),C'O'                                                       
         BNE   *+10                                                             
         MVC   0(2,R3),30(R6)      DMA OF ORIGIN #                              
DMAINFOX XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
STATE    DS    0D                                                               
STATE1   NTR1  BASE=*,LABEL=*                                                   
         MVC   0(2,R3),=C'NA'                                                   
         CLC   =C'CTP',MYDBFILE    COUNTY COVERAGE FILE                         
         BNE   STATEX                                                           
*                                                                               
         L     R6,DBAREC                                                        
         USING DRKEY,R6                                                         
         LARL  RE,STACODE                                                       
STATE5   CLC   =X'FFFF',0(RE)                                                   
         BE    STATEX                                                           
         CLC   DRBTYP,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,28(RE)                                                        
         B     STATE5                                                           
*                                                                               
         CLC   =C'STATE',0(R2)     STATE CODE                                   
         BNE   STATE10                                                          
         MVC   0(2,R3),1(RE)                                                    
         B     STATEX                                                           
*                                                                               
STATE10  CLC   =C'STANM',0(R2)     STATE NAME                                   
         BNE   STATEX                                                           
         MVC   0(25,R3),3(RE)                                                   
*                                                                               
STATEX   XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
BKOUT    DS    0D                                                               
BKOUT1   NTR1  BASE=*,LABEL=*                                                   
         MVI   0(R3),C' '                                                       
         L     R6,DBAREC                                                        
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BKOUTX                                                           
         USING PHTELEM,R6                                                       
         TM    PHTRSF,X'01'                                                     
         BZ    BKOUTX                                                           
         MVI   0(R3),C'B'          IS BREAKOUT                                  
BKOUTX   XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
PGRECD   DS    0D                  EXTRACT PROGRAM RECORD INFO                  
PGREC1   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'EVN',MYDBFILE    COUNTY COVERAGE FILE                         
         BNE   PGRECX                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PGRECX                                                           
         USING NPGEL03,R6                                                       
         CLC   =C'PNEW',0(R2)                                                   
         BNE   *+10                                                             
         MVC   0(1,R3),NPPRNEW                                                  
         CLC   =C'PTIER',0(R2)                                                  
         BNE   *+10                                                             
         MVC   0(1,R3),NPTIER                                                   
         CLC   =C'PRAT',0(R2)                                                   
         BNE   *+10                                                             
         MVC   0(2,R3),NPPRGRAT                                                 
PGRECX   XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
UYEAR    DS    0D                  EXTRACT UNIVERSE YEAR                        
UYEAR1   NTR1  BASE=*,LABEL=*                                                   
         XC    0(2,R3),0(R3)       CLEAR OUTPUT AREA                            
                                                                                
         L     R7,DBCOMFCS         GET V(NETWEEK)                               
         USING COMFACSD,R7                                                      
*                                                                               
         CLC   MYDBFILE,=C'NAD'                                                 
         BE    UYEAR02                                                          
         CLC   MYDBFILE,=C'RLD'                                                 
         BE    UYEAR02                                                          
         CLC   MYDBFILE,=C'NTI'                                                 
         BNE   UYEARX              CURRENTLY VALID FOR NETWORK ONLY             
*                                                                               
UYEAR02  OC    CCALLOV,CCALLOV                                                  
         BZ    UYEARX              CAN'T GET ADDRESS OF NETWEEK                 
         LA    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D9000A17'                                           
         GOTO1 CCALLOV,(R1),0                                                   
         MVC   ANETWEEK,DMCB                                                    
         OC    ANETWEEK,ANETWEEK                                                
         BZ    UYEARX                                                           
*                                                                               
UYEAR03  CLI   DBSELSTA+4,C'I'     FOR IAG USE DBSELBK TO GET YEAR              
         BNE   UYEAR05                                                          
         MVC   RECBOOK,DBSELBK                                                  
         B     UYEAR20                                                          
                                                                                
UYEAR05  L     R6,DBAREC                                                        
         CLI   0(R6),PRCODEQU      P-RECORD                                     
         BNE   UYEAR10                                                          
         USING PRKEY,R6                                                         
         MVC   RECBOOK,PRBOOK      RECBOOK=RECORD BOOK                          
         B     UYEAR20                                                          
         DROP  R6                                                               
                                                                                
UYEAR10  CLI   0(R6),PMCODEQU      Q-RECORD                                     
         BNE   UYEAR15                                                          
         USING PMKEY,R6                                                         
         MVC   RECBOOK,PMBOOK      RECBOOK=RECORD BOOK                          
         B     UYEAR20                                                          
         DROP  R6                                                               
                                                                                
UYEAR15  CLI   0(R6),MXMCODEQ      M-RECORD                                     
         BNE   UYEARX                                                           
         USING MXMKEY,R6                                                        
         MVC   RECBOOK,MXMBOOK     RECBOOK=RECORD BOOK                          
         B     UYEAR20                                                          
         DROP  R6                                                               
                                                                                
UYEAR20  GOTO1 CDEMTABS,DMCB,UNIVYRS GET A(UNIVERSE YEARS TABLE)                
         ICM   R4,15,0(R1)         R4=A(MONTH/DATES TABLE)                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            R0 HAS LENGTH OF TABLE ENTRY                 
         USING UNVYRD,R4                                                        
                                                                                
UYEAR30  CLC   =X'FF',0(R4)                                                     
         BE    UYEARX                                                           
                                                                                
         CLC   =C'NAD',MYDBFILE                                                 
         BNE   UYEAR33                                                          
         ICM   RE,15,DBAREC                                                     
         CLI   1(RE),C'W'          THIS IS WEEKLY NAD                           
         BNE   UYEAR35             MONTHLY FILES SKIP BOOK CONVERSION           
*                                                                               
UYEAR33  GOTO1 CDATCON,DMCB,(4,UYSTDT),(0,DUB),0   CONVERT UNIVERSE BK          
         GOTO1 ANETWEEK,DMCB,DUB,CGETDAY,CADDAY    FOR NETWEEK                  
         MVC   WORK(1),4(R1)                                                    
         MVC   WORK+1(1),8(R1)     WORK = START YEAR + WEEK NO                  
         B     UYEAR40                                                          
                                                                                
UYEAR35  DS    0X                  MONTHLY FILES                                
         MVC   WORK(L'UYSTMTH),UYSTMTH   WORK = START YEAR + MONTH NO           
                                                                                
UYEAR40  CLC   RECBOOK,WORK                                                     
         BNL   UYEAR50                                                          
         AR    R4,R0                                                            
         B     UYEAR30                                                          
                                                                                
UYEAR50  MVC   0(2,R3),UYYEAR      OUTPUT UNIVERSE YEAR                         
                                                                                
UYEARX   XIT1                                                                   
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* ROUTINE TO RETURN TIME ROUNDED TO THE NEAREST HH OR QH (FOR CABLE)            
* R5 POINTS TO MILITARY TIME TO ROUND                                           
* REPLACE IT WITH ROUNDED MILITARY TIME                                         
*---------------------------------------------------------------------*         
RNDTIME  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,0(R5)                                                       
         LHI   RE,100                                                           
         DR    R0,RE               GET R0=MINUTES, R1=HOURS                     
                                                                                
         L     RE,DBAREC           FOR CABLE ROUND TO NEAREST 15 MIN            
         USING PRKEY,RE                                                         
         CLI   PRCODE,MXMCODEQ     C'M' MINUTE BY MINUTE DATA                   
         BE    RDT05                                                            
         CLI   PRCODE,PRCODEQU     C'P' TIME PERIOD DATA                        
         BE    RDT05                                                            
         CLI   PRCODE,PMCODEQU     C'Q' PROGRAM DATA                            
         BNE   RDT10                                                            
RDT05    CLI   PRMEDIA,C'C'        CABLE                                        
         BE    RDT20                                                            
         CLI   PRMEDIA,C'c'        CABLE COMMERCIAL AVERAGE                     
         BE    RDT20                                                            
         DROP  RE                                                               
                                                                                
RDT10    CHI   R0,45               ALL OTHERS ROUND TO NEAREST 30 MIN           
         BL    RDT11                                                            
         AHI   R1,1                ADJUST THE HOUR                              
         CHI   R1,24                                                            
         BNH   RDT12                                                            
         SHI   R1,24                                                            
         B     RDT12                                                            
RDT11    CHI   R0,15                                                            
         BNL   RDT14                                                            
RDT12    SR    R0,R0               45->14 ROUND TO 00                           
         B     RDT50                                                            
                                                                                
RDT14    CHI   R0,15                                                            
         BL    RDT16                                                            
         CHI   R0,45                                                            
         BNL   RDT16                                                            
         LHI   R0,30               15->44 ROUND TO 30                           
         B     RDT50                                                            
RDT16    DC    H'0'                ALL OPTIONS SHOULD HAVE BEEN COVERED         
                                                                                
RDT20    CHI   R0,53               FOR CABLE ROUND TO NEAREST 15 MIN            
         BL    RDT21                                                            
         AHI   R1,1                ADJUST HOUR                                  
         CHI   R1,24                                                            
         BNH   RDT22                                                            
         SHI   R1,24                                                            
         B     RDT22                                                            
RDT21    CHI   R0,7                                                             
         BH    RDT24                                                            
RDT22    SR    R0,R0               53->07 ROUND TO 00                           
         B     RDT50                                                            
                                                                                
RDT24    CHI   R0,8                                                             
         BNL   *+6                                                              
         DC    H'0'                                                             
         CHI   R0,22                                                            
         BH    RDT26                                                            
         LHI   R0,15               08->22 ROUND TO 15                           
         B     RDT50                                                            
                                                                                
RDT26    CHI   R0,23                                                            
         BNL   *+6                                                              
         DC    H'0'                                                             
         CHI   R0,37                                                            
         BH    RDT28                                                            
         LHI   R0,30               23->37 ROUND TO 30                           
         B     RDT50                                                            
                                                                                
RDT28    CHI   R0,38                                                            
         BNL   *+6                                                              
         DC    H'0'                                                             
         CHI   R0,52                                                            
         BH    RDT30                                                            
         LHI   R0,45               38->52 ROUND TO 45                           
         B     RDT50                                                            
                                                                                
RDT30    DC    H'0'                ALL OPTIONS SHOULD HAVE BEEN COVERED         
                                                                                
RDT50    LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LHI   R1,24                                                            
         MHI   R1,100                                                           
         AR    R1,R0                                                            
         STCM  R1,3,0(R5)          STORE ROUNDED MILITARY TIME                  
                                                                                
RDTX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO RETURN THE LONG NETWORK NAME                                       
*---------------------------------------------------------------------*         
LNET     NTR1  BASE=*,LABEL=*                                                   
         MVC   0(30,R3),=CL30' '    DEFAULT IS BLANK NAME                       
         L     R6,DBAREC                                                        
         CLI   0(R6),PMCODEQU      'Q' - PROGRAM RECORDS                        
         BE    LNET10                                                           
         CLI   0(R6),PRCODEQU      'P' - TIME PERIOD RECORDS                    
         BE    LNET20                                                           
         CLI   0(R6),MXMCODEQ      'M' - MINUTE BY MINUTE RECORDS               
         BE    LNET25                                                           
         B     LNETX                                                            
                                                                                
         USING PMKEY,R6                                                         
LNET10   MVC   DUB(1),PMMEDIA                                                   
         MVC   DUB+1(1),PMSRC                                                   
         MVC   DUB+2(5),PMSTAT                                                  
         MVC   DUB2(2),PMBOOK                                                   
         BRAS  RE,OOHSRC           CONVERT OOH SOURCE                           
         B     LNET30                                                           
                                                                                
         USING PRKEY,R6                                                         
LNET20   MVC   DUB(1),PRMEDIA                                                   
         MVC   DUB+1(1),PRSRC                                                   
         MVC   DUB+2(5),PRSTAT                                                  
         MVC   DUB2(2),PRBOOK                                                   
         BRAS  RE,OOHSRC           CONVERT OOH SOURCE                           
         B     LNET30                                                           
         DROP  R6                                                               
                                                                                
         USING MXMKEY,R6                                                        
LNET25   MVC   DUB(1),MXMMEDIA                                                  
         MVC   DUB+1(1),MXMSRC                                                  
         MVC   DUB+2(5),MXMSTAT                                                 
         MVC   DUB2(2),MXMBOOK                                                  
         BRAS  RE,OOHSRC           CONVERT OOH SOURCE                           
         B     LNET30                                                           
         DROP  R6                                                               
                                                                                
LNET30   TM    DUB+1,X'F0'         SOURCE WAS ALTERED FOR TIME SHIFTED          
         BNO   *+8                                                              
         MVI   DUB+1,C'N'          ADJUST SOURCE TO NIELSEN                     
                                                                                
         CLI   DUB+6,C'h'          STATION IS ALTERED FOR                       
         BNE   *+8                 WEIGHTED HISPANIC                            
         MVI   DUB+6,C'H'          ADJUST TO UPPER CASE 'H'                     
                                                                                
         CLC   =C'PAR',DUB+2       HANDLE NAME CHANGE FROM PAR TO UPN           
         BNE   *+10                                                             
         MVC   DUB+2(3),=C'UPN'                                                 
                                                                                
         CLC   =C'PAX',DUB+2       HANDLE NAME CHANGE FROM PAX TO ION           
         BNE   *+10                                                             
         MVC   DUB+2(3),=C'ION'                                                 
                                                                                
         CLC   =C'TF ',DUB+2       HANDLE NAME CHANGE FROM TF TO UMA            
         BNE   *+10                                                             
         MVC   DUB+2(3),=C'UMA'                                                 
                                                                                
         CLC   =C'MFX',DUB+2       HANDLE NAME CHANGE FROM MFX TO MMX           
         BNE   *+10                                                             
         MVC   DUB+2(3),=C'MMX'                                                 
                                                                                
         CLC   =AL1(YR_2010,WEEK_40)(2),DUB2       BEFORE SEP27/10,             
         BNH   LNET32                                                           
         CLI   DUB+6,C'S'          UNLESS IT'S SYNDICATION,                     
         BE    LNET32                                                           
         CLC   =C'PBS',DUB+2       CALL LETTERS PBS REALLY REPRESENT            
         BNE   LNET32                                                           
         MVC   DUB+2(3),=C'PBA'     THE PBS AFFILIATES (PBA)                    
                                                                                
LNET32   CLI   DUB,C'A'            ADJUST LOWER CASE MEDIA FOR ACM              
         BNL   LNET33                                                           
         ZIC   R0,DUB                                                           
         AHI   R0,X'40'                                                         
         STC   R0,DUB              ADJUST TO UPPER CASE                         
                                                                                
LNET33   LARL  R7,LNETTAB          DUB=MEDIA(1),SOURCE(1),NETWORK(5)            
                                                                                
         USING LNETTABD,R7                                                      
LNET35   CLI   0(R7),X'FF'         FIND CORRESPONDING TABLE OF NETWKS           
         BE    LNETX                                                            
         CLC   LNETMED,DUB         COMPARE ON MEDIA                             
         BNE   LNET40                                                           
         CLC   LNETSRC,DUB+1       COMPARE ON SOURCE                            
         BNE   LNET40                                                           
         CLC   LNETC5,DUB+2+4      COMPARE ON 5TH CHAR OF THE NET CODE          
         BNE   LNET40                                                           
         B     LNET50                                                           
                                                                                
LNET40   LA    R7,LNETTABL(R7)                                                  
         B     LNET35                                                           
                                                                                
LNET50   OC    LNETATAB(3),LNETATAB  CK IF TABLE LIVES IN DEMTABS               
         BNZ   LNET54                                                           
         ZIC   R5,LNETATAB+3       YES. GRAB PARAMETER TO DEMTABS               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,(R5)                                                   
         ICM   R6,15,0(R1)         R6=A(TABLE OF CABLE NET NAMES)               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,4(R1)            LENGTH OF TABLE ENTRY                        
         STC   RE,LNETLEN          FILL IN LENGTH IN TABLE                      
         B     LNET54A                                                          
                                                                                
LNET54   ICM   R6,15,LNETATAB      R6=CORRESPONDING TABLE OF NET NAMES          
         A     R6,RELO                                                          
                                                                                
LNET54A  OC    LNETAROU,LNETAROU   MAKE NETW CODE SAME FORMAT AS TABLE          
         BZ    LNET55                                                           
         ICM   RF,15,LNETAROU                                                   
         A     RF,RELO                                                          
         BASR  RE,RF               GET EDITED NETW CODE IN DUB+2                
         BNE   LNET40              BAD ROUTINE FOR RECD. TRY NEXT TABLE         
                                                                                
LNET55   CLI   0(R6),X'FF'         NETWORK NOT FOUND                            
         BE    LNET40              TRY NEXT TABLE (EX, AGGREGATES)              
                                                                                
LNET60   ZIC   RE,LNETNETD         DISPLACEMENT TO NETWK CODE                   
         AR    RE,R6                                                            
         ZIC   R1,LNETNLEN         ACTUAL LENGTH OF NETW CODE                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DUB+2(0),0(RE)      COMPARE ON NETWORK CODE                      
         BE    LNET70                                                           
         ZIC   RE,LNETLEN                                                       
         AR    R6,RE                                                            
         B     LNET55                                                           
                                                                                
LNET70   ZIC   RE,LNETNAMD                                                      
         AR    RE,R6                                                            
         MVC   0(30,R3),0(RE)                                                   
                                                                                
LNETX    J     XIT                                                              
         DROP  R7                                                               
*---------------------------------------------------------------------*         
* ROUTINE TO CONVERT OOH SOURCE FOR LNET TABLE LOOKUP                           
*---------------------------------------------------------------------*         
OOHSRC   NTR1  BASE=*,LABEL=*                                                   
         CLI   DUB+1,SRCLOLQ                                                    
         BNE   *+8                                                              
         MVI   DUB+1,C'N'                                                       
         CLI   DUB+1,SRCLOSQ                                                    
         BNE   *+8                                                              
         MVI   DUB+1,C'N'                                                       
         CLI   DUB+1,SRCLO3Q                                                    
         BNE   *+8                                                              
         MVI   DUB+1,C'N'                                                       
         CLI   DUB+1,SRCLO7Q                                                    
         BNE   *+8                                                              
         MVI   DUB+1,C'N'                                                       
         CLI   DUB+1,SRCLOCLQ                                                   
         BNE   *+8                                                              
         MVI   DUB+1,C'N'                                                       
         CLI   DUB+1,SRCLOCSQ                                                   
         BNE   *+8                                                              
         MVI   DUB+1,C'N'                                                       
         CLI   DUB+1,SRCLOC3Q                                                   
         BNE   *+8                                                              
         MVI   DUB+1,C'N'                                                       
         CLI   DUB+1,SRCLOC7Q                                                   
         BNE   *+8                                                              
         MVI   DUB+1,C'N'                                                       
         J     XIT                                                              
*---------------------------------------------------------------------*         
* ROUTINE TO EDIT CABLE NUMERIC CODE                                            
* AT ENTRY, DUB+2(4) HOLDS STATION FROM THE DEMO RECD                           
* AT EXIT, DUB+2(3) WILL HOLD THE NETWORK CODE AS IT APPEARS IN TABLE           
* CC= EQUAL IF EDITING WORKED FINE                                              
*---------------------------------------------------------------------*         
ECABNET  NTR1  BASE=*,LABEL=*                                                   
         CLI   DUB+2,C'H'                                                       
         JE    EDITBAD             MUST BE HISPANIC CABLE. EXIT                 
                                                                                
         CLI   DUB+2,0             IF NOT ALPHANUMERIC                          
         BNE   ECAB10                                                           
         SR    RE,RE               MUST BE 3-BYTE BINARY                        
         ICM   RE,7,DUB+3                                                       
         STCM  RE,7,DUB+2                                                       
         MVI   DUB+5,0                                                          
         J     EDITOK                                                           
                                                                                
ECAB10   PACK  DUB2,DUB+2(4)       ALPHANUMERIC STATION                         
         CVB   R0,DUB2                                                          
         STCM  R0,7,DUB+2                                                       
         MVI   DUB+5,0                                                          
         J     EDITOK                                                           
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO EDIT CABLE NUMERIC CODE FOR RENTRAK                                
* AT ENTRY, DUB+2(4) HOLDS ALPHA-NUMERIC STATION FROM THE DEMO RECD             
* AT EXIT, DUB+2(2) WILL HOLD THE BINARY NETWORK CODE                           
* CC= EQUAL IF EDITING WORKED FINE.                                             
*---------------------------------------------------------------------*         
ECABREN  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         PACK  DUB2,DUB+2(4)       ALPHANUMERIC STATION                         
         CVB   R0,DUB2                                                          
         STCM  R0,3,DUB+2          BINARY STATION                               
         J     EDITOK                                                           
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO EDIT CABLE NUMERIC CODE                                            
* AT ENTRY, DUB+2(4) HOLDS NETWORK CODE FROM THE DEMO RECD: H(PWOS)             
*           WHERE THE PWOS NUMBER IS 3-BYTES                                    
* AT EXIT, DUB+2(3) WILL HOLD THE PWOS NETWORK CODE                             
*---------------------------------------------------------------------*         
ENHCNET  NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         ICM   R0,7,DUB+3                                                       
         STCM  R0,7,DUB+2                                                       
         MVI   DUB+5,0                                                          
         J     EDITOK                                                           
                                                                                
EDITOK   CR    RE,RE                                                            
         J     XIT                                                              
EDITBAD  CHI   RB,0                                                             
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
* GET TRACK NAME                                                                
*---------------------------------------------------------------------*         
TRAKNTR  NTR1  BASE=*,LABEL=*                                                   
         CLC   MYDBFILE,=C'IUN'                                                 
         BE    TRAK10                                                           
         LA    RF,15               DEFAULT OUTPUT LEN = 16                      
         CLC   0(6,R2),=C'TRAK25'                                               
         BNE   *+8                                                              
         LA    RF,24               OPTION FOR 25 FOR SOME FILES                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=CL25' '                                                 
         LA    R0,1(RF)                                                         
         CLC   MYDBFILE,=C'NTI'                                                 
         BE    TRAK2                                                            
*                                                                               
         CLC   MYDBFILE,=C'RLD'    RLD (MINUTE BY MINUTE)                       
         BNE   TRAKNX                                                           
         L     RE,DBAREC                                                        
         USING MXMKEY,RE                                                        
         CLI   MXMMEDIA,C'C'                                                    
         BNE   TRAK2A              IF NOT CABLE, USE PROGRAM NAME               
         DROP  RE                                                               
         MVI   ELCODE,NTRKCDEQ     IF CABLE USE TRACKAGE ELEMENT '28'           
         BRAS  RE,GETEL                                                         
         BNE   TRAKNX                                                           
         USING NTRKELEM,R6                                                      
         LA    R9,NTRKNAME                                                      
         ZIC   R1,NTRKLN                                                        
         SHI   R1,NTRKNAME-NTRKELEM                                             
         BCTR  R1,0                                                             
         CR    R1,R0               TEST FOR LONG TRACK NAMES                    
         BH    TRAK3                                                            
         EX    R1,*+8                                                           
         B     TRAKNX                                                           
         MVC   0(0,R3),NTRKNAME                                                 
         DROP  R6                                                               
                                                                                
TRAK2    ICM   RE,15,DBSPANAD      CHECK FOR SPANNED RECORDS                    
         BNZ   TRAK4                                                            
TRAK2A   MVI   ELCODE,X'21'        PAVFILE                                      
         BRAS  RE,GETEL                                                         
         BNE   TRAKNX                                                           
         USING PPTELEM,R6                                                       
         LA    R9,PPTTITLE                                                      
         ZIC   R1,PPTELN                                                        
TRAK2B   SHI   R1,3                                                             
         CR    R1,R0               TEST FOR LONG TRACK NAMES                    
         BH    TRAK3                                                            
         EX    R1,*+8                                                           
         B     TRAKNX                                                           
         MVC   0(0,R3),PPTTITLE                                                 
         SPACE 1                                                                
TRAK3    LA    RF,1(R1)            RESTORE PROGRAM NAME LENGTH                  
         GOTO1 =V(MININAM),DMCB,((RF),(R9)),((R0),(R3)),               X        
               =C'PNABBR',RR=RELO                                               
         B     TRAKNX                                                           
*                                                                               
TRAK4    CLC   0(3,RE),=C'CB2'     CABLE NAD AND REG CABLE                      
         BE    *+10                                                             
         CLC   0(3,RE),=C'CAB'     IF NOT CABLE - FORGET IT                     
         BNE   TRAK2A               JUST DO NORMAL STUFF                        
         CLI   3(RE),1                                                          
         BL    TRAKNX                                                           
         CLC   4(30,RE),34(RE)     SAME TRACK/PROGRAM                           
         BE    TRAKNX              JUST EXIT                                    
         LA    R1,25                                                            
         LA    R9,34(RE)                                                        
         B     TRAK3                                                            
*                                                                               
TRAK10   DS    0H                  FOOTNOTE FOR REP INV                         
         ICM   RE,15,DBEXTEND                                                   
TRAK12   BZ    TRAKNX                                                           
         CLC   0(4,RE),=C'RINV'                                                 
         BE    *+12                                                             
         ICM   RE,15,4(RE)                                                      
         B     TRAK12                                                           
                                                                                
         USING DBXINVWK,RE                                                      
         MVC   0(L'DBXIFOOT,R3),DBXIFOOT                                        
         B     TRAKNX                                                           
         DROP  RE                                                               
                                                                                
TRAKNX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO RETURN RECORD LOAD DATE                                            
*---------------------------------------------------------------------*         
LDDAT    NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    0(2,R3),0(R3)                                                    
                                                                                
         L     R6,DBAREC                                                        
         MVC   DATADISP,=AL2(PRFRSTEL-PRKEY) START OF RECD,NOT DBAQUART         
                                                                                
         MVI   ELCODE,MARCODEQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   LDDATX                                                           
                                                                                
         USING MARELEM,R6                                                       
         IF (CLI,MARELN,GE,MARLNEQ3)                                            
           L     RF,DBCOMFCS                                                    
           L     RF,CDATCON-COMFACSD(,RF)                                       
           GOTO1 (RF),DMCB,(3,MARDATE3),(2,0(R3))                               
         ELSE ,                                                                 
* VERY IMPORTANT: A MONTH OR SO BEFORE D-DAY, THIS ENTIRE "ELSE" CLAUSE         
*  SHOULD BE REMOVED, BECAUSE AFTER D-DAY, FIELD "MARDATE" WILL BE AN           
*  *UNCONVERTED* TYPE-2 DATE, AND THEREFORE INVALID. ONLY *MARDATE3*            
*  SHOULD BE USED. THIS APPROACH IS OKAY, BECAUSE ANY RECORD LOADED IN          
*  THE 2 OR 3 YEARS PRIOR TO D-DAY WILL HAVE MARDATE3 POPULATED. ALSO,          
*  MARIA SAYS THAT THIS DEFINE CALL IS ONLY USED INTERNALLY, SO EVEN IF         
*  WE DON'T RETURN A LOAD DATE FOR AN OLD RECORD, NO CLIENT WILL                
*  COMPLAIN.                                                                    
           FIXDT02 ,                                                            
           MVC   0(2,R3),MARDATE   **INVALID AFTER D-DAY!!!**                   
         ENDIF ,                                                                
                                                                                
LDDATX   J     XIT                                                              
         DROP  R6                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO LONG WEEKS FIELD (MAX OF 5 WEEKS INSTEAD OF 4 WEEKS)               
*---------------------------------------------------------------------*         
LWEEK    NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   0(7,R3),=CL16' '                                                 
         CLI   CBLNAD,1           CABLE NAD                                     
         BE    LWEEK05                                                          
         CLC   MYDBFILE,=C'NAD'   AND ALL NAD SOURCES (INCLUDES NHT)            
         BNE   LWEEKX                                                           
                                                                                
LWEEK05  MVI   ELCODE,NTCODEQU    R6 POINTS TO DEMO RECORD OR DBSPANAD          
         BRAS  RE,GETEL           LOOK FOR LONG WEEK FIELD                      
         BNE   LWEEK10            NOT AVAILABLE.USE SHORT WEEK                  
         USING NTELEM,R6                                                        
         CLI   NTLEN,NTLENEQ2                                                   
         BL    LWEEK10            NOT AVAILABLE.USE SHORT WEEK                  
         OC    NTWKS2,NTWKS2                                                    
         BZ    LWEEK10            NOT FILLED IN                                 
         MVC   0(1,R3),NTWKS2                                                   
         MVI   6(R3),C'L'         LONG WEEK INDICATOR                           
         B     LWEEK20                                                          
         DROP  R6                                                               
                                                                                
LWEEK10  LR    R6,R7               START AT DBAQUART                            
         CLI   0(R6),PHCODEQ       MAKE SURE I HAVE THE RIGHT ELEMENT           
         BNE   LWEEKX                                                           
         USING PHELEM,R6                                                        
         MVC   0(1,R3),PHDWKS      EXTRACT WEEK BITS                            
         NI    0(R3),X'0F'         CLEAR TYP/HIGHEST BITS                       
         ZIC   RE,0(R3)                                                         
         SLL   RE,4                FORMAT WEEK1 FROM X'08' TO X'80'             
         STC   RE,0(R3)                                                         
         DROP  R6                                                               
                                                                                
LWEEK20  TM    0(R3),X'80'                                                      
         BNO   *+8                                                              
         MVI   1(R3),C'1'                                                       
         TM    0(R3),X'40'                                                      
         BNO   *+8                                                              
         MVI   2(R3),C'2'                                                       
         TM    0(R3),X'20'                                                      
         BNO   *+8                                                              
         MVI   3(R3),C'3'                                                       
         TM    0(R3),X'10'                                                      
         BNO   *+8                                                              
         MVI   4(R3),C'4'                                                       
         TM    0(R3),X'08'                                                      
         BNO   *+8                                                              
         MVI   5(R3),C'5'          UP TO 5 WEEKS IN A MONTH                     
         B     LWEEKX                                                           
                                                                                
LWEEKX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO EXTRACT THE NUMBER OF CONTRIBUTING COMMERCIAL SECONDS FROM         
* THE COMMERCIAL AVERAGE DATA                                                   
*---------------------------------------------------------------------*         
COMSEC   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    0(2,R3),0(R3)                                                    
         MVI   ELCODE,NTCODEQU     RUN TIME ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   COMSECX                                                          
         USING NTELEM,R6                                                        
         CLI   NTLEN,NTLENEQ3      COMMERCIAL AVG INFO NOT AVAILABLE            
         BL    COMSECX                                                          
         MVC   0(L'NTCMSEC,R3),NTCMSEC     COMMERCIAL AVERAGE SECONDS           
         DROP  R6                                                               
                                                                                
COMSECX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO EXTRACT THE NUMBER OF CONTRIBUTING TELECASTS FROM                  
* THE COMMERCIAL AVERAGE DATA                                                   
*---------------------------------------------------------------------*         
COMTEL   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   0(R3),0                                                          
         MVI   ELCODE,NTCODEQU     RUN TIME ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   COMTELX                                                          
         USING NTELEM,R6                                                        
         CLI   NTLEN,NTLENEQ3      COMMERCIAL AVG INFO NOT AVAILABLE            
         BL    COMTELX                                                          
         MVC   0(L'NTCMTEL,R3),NTCMTEL   COMMERCIAL AVERAGE TELECASTS           
         DROP  R6                                                               
                                                                                
COMTELX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO EXTRACT THE PRORAM MINUTE FROM THE MINUTE BY MINUTE DATA           
*---------------------------------------------------------------------*         
PROGMIN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    0(2,R3),0(R3)                                                    
         MVI   ELCODE,MNTCODEQ     MINUTE ELEMENT                               
         BRAS  RE,GETEL                                                         
         BNE   PROGMINX                                                         
         USING MNTELEM,R6                                                       
         MVC   0(L'MNTMIN,R3),MNTMIN                                            
         DROP  R6                                                               
*                                                                               
PROGMINX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO EXTRACT THE PRORAM DURATION IN MINUTES                             
*---------------------------------------------------------------------*         
DURATION NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    0(2,R3),0(R3)                                                    
         MVI   ELCODE,NTCODEQU     PROGRAM RUN TIME ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   DURATINX                                                         
         USING NTELEM,R6                                                        
         MVC   1(1,R3),NTDUR       1-BYTE DURATION                              
         CLI   NTLEN,NTLENEQ2                                                   
         BL    DURATINX                                                         
         MVC   0(2,R3),NTDUR2      2-BYTE DURATION WHEN AVAILABLE               
         DROP  R6                                                               
*                                                                               
DURATINX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO EXTRACT COMMERCIAL/PROMO/PSA SECONDS DURATION IN A MINUTE          
*---------------------------------------------------------------------*         
MCOMSEC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    0(4,R3),0(R3)                                                    
         MVI   ELCODE,MNTCODEQ     MINUTE ELEMENT                               
         BRAS  RE,GETEL                                                         
         BNE   MCOMSECX                                                         
         USING MNTELEM,R6                                                       
         MVC   0(1,R3),MNTCOMM     COMMERCIAL MINUTE FLAG:Y,N,U                 
         CLI   MNTLEN,MNTLEN1Q                                                  
         BNH   MCOMSECX                                                         
         MVC   1(1,R3),MNTCMSEC    COMMERCIAL SECONDS                           
         MVC   2(1,R3),MNTPRSEC    PROMO SECONDS                                
         MVC   3(1,R3),MNTPSSEC    PSA SECONDS                                  
         DROP  R6                                                               
*                                                                               
MCOMSECX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO EXTRACT POD INFORMATION FOR THE COMMERCIAL MINUTES                 
*---------------------------------------------------------------------*         
PODINFO  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    0(13,R3),0(R3)                                                   
         MVI   ELCODE,MNTCODEQ     MINUTE ELEMENT                               
         BRAS  RE,GETEL                                                         
         BNE   PODINFOX                                                         
         USING MNTELEM,R6                                                       
         CLI   MNTLEN,MNTLEN1Q                                                  
         BNH   PODINFOX                                                         
         MVC   0(2,R3),MNTPODST    POD START TIME IN HHMM FORMAT                
         MVC   4(2,R3),MNTPODMN    POD LENGTH IN MINUTES                        
         MVC   6(4,R3),MNTPODSC    POD TOTAL COMMERCIAL SECONDS                 
         MVC   10(2,R3),MNTPODNO   POD NUMBER WITHIN PROG/TELECAST              
         MVC   12(1,R3),MNTFL      FIRST/LAST MINUTE FLAG                       
         MVC   13(4,R3),MNTPODTS   POD TOTAL SECONDS                            
*                                                                               
         SR    R0,R0               COMPUTE POD END TIME FROM START TIME         
         SR    R1,R1                AND MINUTE DURATION                         
         ICM   R1,3,MNTPODST       START TIME                                   
         D     R0,=F'100'          MINUTES IN R0, HOURS IN R1                   
         SR    RF,RF                                                            
         ICM   RF,3,MNTPODMN       DURATION IN MINUTES                          
         SHI   RF,1                -1                                           
         SR    RE,RE                                                            
         D     RE,=F'60'           MINUTES IN RE, HOURS IN RF                   
         AR    R0,RE               ADD MINUTES TOGETHER                         
         CHI   R0,60               TEST IF SUM GT OR EQ TO 1 HOUR               
         BL    *+12                NO                                           
         SHI   R0,60               SUBTRACT 60 MINUTES                          
         LA    RF,1(RF)            AND ADD 1 TO HOURS                           
         AR    R1,RF               ADD HOURS TOGETHER                           
         MHI   R1,100              HOURS X 100 FOR MILITARY TIME                
         AR    R1,R0               ADD MINUTES TO HOURS                         
         STCM  R1,3,2(R3)          POD END TIME                                 
*                                                                               
         DROP  R6                                                               
*                                                                               
PODINFOX J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO EXTRACT GAPPED PROGRAM INDICATOR                                   
*---------------------------------------------------------------------*         
GAPIND   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   0(R3),C' '                                                       
         MVI   ELCODE,NTCODEQU     PROGRAM RUN TIME ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   GAPINDX                                                          
         USING NTELEM,R6                                                        
         CLI   NTLEN,NTLENEQ                                                    
         BL    GAPINDX                                                          
         CLI   NTGAP,0                                                          
         BE    GAPIND                                                           
         MVC   0(1,R3),NTGAP                                                    
         DROP  R6                                                               
*                                                                               
GAPINDX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO EXTRACT THE MINUTE OF PROGRAM (MOP)                                
*---------------------------------------------------------------------*         
MOP      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    0(L'MNTMOP,R3),0(R2)                                             
         MVI   ELCODE,MNTCODEQ     MINUTE ELEMENT                               
         BRAS  RE,GETEL                                                         
         BNE   MOPX                                                             
         USING MNTELEM,R6                                                       
         MVC   0(L'MNTMOP,R3),MNTMOP                                            
         DROP  R6                                                               
*                                                                               
MOPX     J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO EXTRACT THE TRACK AND TELECAST NUMBERS                             
*---------------------------------------------------------------------*         
TRKTELNO NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   0(R3),0                                                          
         MVC   1(10,R3),=CL10' '                                                
*                                                                               
         ICM   RE,15,DBSPANAD      IF DBSPANAD AVAILABLE,                       
         BZ    TRKTEL10             USE INFORMATION FROM HERE                   
         USING SPANADD,RE                                                       
         CLC   SPDID,=C'CB2'                                                    
         BE    *+10                                                             
         CLC   SPDID,=C'CAB'                                                    
         BNE   TRKTEL10                                                         
         MVC   0(1,R3),SPDLEVEL    LEVEL (0=PG AVG,1=TRK,2=TCAST)               
         MVC   1(5,R3),SPDTRNUM+5  TRACK NUMBER                                 
         MVC   6(5,R3),SPDTCNUM+5  TELECAST NUMBER                              
         B     TRKTELX                                                          
         DROP  RE                                                               
*                                                                               
TRKTEL10 MVI   ELCODE,NTRKCDEQ     TRACK ELEMENT                                
         BRAS  RE,GETEL                                                         
         BNE   TRKTEL20                                                         
         USING NTRKELEM,R6                                                      
         EDIT  NTRKNUM,(5,1(R3)),ZERO=BLANK,FILL=0                              
         DROP  R6                                                               
                                                                                
TRKTEL20 MVI   ELCODE,NTCODEQU                                                  
         L     R6,DBAREC                                                        
         BRAS  RE,GETEL                                                         
         BNE   TRKTELX                                                          
         USING NTELEM,R6                                                        
         EDIT  NTTNUM,(5,6(R3)),ZERO=BLANK,FILL=0                               
         DROP  R6                                                               
         MVI   0(R3),2             ALWAYS TELECAST LEVEL                        
*                                                                               
TRKTELX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* ROUTINE TO OVERRIDE A SET OF SPECIAL ITN PROGRAM NAMES.                       
* ITN PROVIDED A LIST OF PROGRAMS THAT THEY WANTED TO OVERRIDE THE              
* NAMES FOR. THE LIST OF PROGRAMS AND THEIR OVERRIDE NAMES LIVES IN             
* DEMTABS.                                                                      
*                                                                               
* OF PROGRAM NAME HAS AN OVERRIDE, ON EXIT:                                     
*          WORK(25) HAS THE OVERRIDE PROGRAM NAME                               
*          R1 HAS THE LENGTH OF THE OVERRIDEN PROGRAM NAME, EXCLUDING           
*          SPACES.                                                              
*---------------------------------------------------------------------*         
ITNPRGOV NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'ITN',DBSELSTA                                                 
         BNE   ITNPRGOX                                                         
*                                                                               
         L     R6,DBAREC                                                        
         MVI   ELCODE,PHCODEQ      GET NTI NUMBER FROM X'20' ELEMENT            
         BRAS  RE,GETEL                                                         
         BNE   ITNPRGOX                                                         
         USING PHELEM,R6                                                        
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,ITNPNAME  GET ITN PROGRAM NAMES TABLE                  
         ICM   RE,15,0(R1)         RE=A(ITN PROGRAM NAMES TABLE)                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R5,4(R1)            R5=LENGTH OF TABLE ENTRY                     
                                                                                
         USING ITNPNAMD,RE                                                      
ITNPO10  CLI   0(RE),X'FF'                                                      
         BE    ITNPRGOX            OVERRIDE PROGRAM NAME NOT IN TABLE           
         CLC   PHPNUM,ITNPNTI                                                   
         BE    ITNPO20             MATCH ON NTI NUMBER                          
         AR    RE,R5                                                            
         B     ITNPO10             TRY NEXT ENTRY IN TABLE                      
*                                                                               
ITNPO20  MVC   WORK(L'ITNPNAM),ITNPNAM                                          
         LHI   R1,L'ITNPNAM                                                     
         LA    RE,ITNPNAM+L'ITNPNAM-1                                           
ITNPO30  CLI   0(RE),C' '                                                       
         BNE   ITNPRGX1                                                         
         SHI   RE,1                BACK UP 1 CHARACTER                          
         BCT   R1,ITNPO30          DECREMENT LENGTH                             
*                                                                               
ITNPRGOX J     XIT                 EXIT WITHOUT OVERRIDING                      
*                                                                               
ITNPRGX1 XIT1  REGS=(R1)           EXIT WITH NEW LENGTH IN R1                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
* COUNT THE NUMBER OF DAYS FOR NATIONAL BROADCAST AND SYND PROGRAM DATA         
*---------------------------------------------------------------------*         
CNTDAYS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   0(R3),0                                                          
         L     R6,DBAREC                                                        
*                                                                               
         USING PMKEY,R6                                                         
         CLI   PMCODE,PMCODEQU     MAKE SURE IT'S NATIONAL PROGRAM DATA         
         BNE   CNTDAYSX                                                         
         CLI   PMMEDIA,C'N'        NETWORK BROADCAST/SYNDICATION                
         BE    *+12                                                             
         CLI   PMMEDIA,C'n'        NETWORK COMMERCIAL AVERAGE                   
         BNE   CNTDAYSX                                                         
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,NTCODEQU     FIND FIRST PROGRAM RUN TIME ELEMENT          
         BRAS  RE,GETEL                                                         
         BNE   CNTDAYSX                                                         
*                                                                               
         USING NTELEM,R6                                                        
         SR    R0,R0               R0 COUNTS # OF DAYS                          
         ZICM  RF,NTDAY,(8)                                                     
         SLL   RF,1                HIGH-ORDER-BIT NOT USED IN DAY CODE          
CNTDAY10 LTR   RF,RF                                                            
         BZ    CNTDAY20                                                         
         SR    RE,RE                                                            
         SLDL  RE,1                                                             
         AR    R0,RE                                                            
         B     CNTDAY10                                                         
CNTDAY20 STC   R0,0(R3)            STORE NUMBER OF DAYS                         
         DROP  R6                                                               
*                                                                               
CNTDAYSX J     XIT                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
OSUSTAIN DS    0D                                                               
OSUST1   NTR1  BASE=*,LABEL=*                                                   
         MVI   0(R3),C' '                                                       
         L     R6,DBAREC                                                        
         MVI   ELCODE,PHTCODEQ     X'10' ELEMENT                                
         BRAS  RE,GETEL                                                         
         BNE   OSUST1X                                                          
         USING PHTELEM,R6                                                       
         CLI   PHTLEN,PHTINDS-PHTELEM                                           
         BNH   OSUST1X                                                          
         TM    PHTINDS,PHTOSQ                                                   
         BZ    OSUST1X                                                          
         MVI   0(R3),C'Y'          IS ORDERED SUSTAINER                         
OSUST1X  XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
                                                                                
DAYTAB   DS    0CL5                                                             
         DC    X'7C',C'M-F',X'05'  0                                            
         DC    X'40',C'MON',X'01'  1                                            
         DC    X'20',C'TUE',X'01'  2                                            
         DC    X'10',C'WED',X'01'  3                                            
         DC    X'08',C'THU',X'01'  4                                            
         DC    X'04',C'FRI',X'01'  5                                            
         DC    X'02',C'SAT',X'01'  6                                            
         DC    X'01',C'SUN',X'01'  7                                            
         DC    X'7F',C'M-S',X'07'  8                                            
         DC    X'7F',C'VAR',X'07'  9                                            
         DC    X'7F',C'VAR',X'07'  A                                            
         DC    X'7F',C'VAR',X'07'  B                                            
         DC    X'7F',C'VAR',X'07'  C                                            
         DC    X'7F',C'TYP',X'07'  D                                            
         DC    X'03',C'WKE',X'02'  E                                            
         DC    X'7F',C'VAR',X'07'  F                                            
         SPACE 1                                                                
         DC    X'FF',C'VAR',X'07'  TABLE TERMINATOR (ADYTM FUNCTION)            
         DS    0H                                                               
         EJECT                                                                  
* DESTACODE                                                                     
       ++INCLUDE DESTACODE                                                      
         EJECT                                                                  
* DERDAYPRT                                                                     
         DS    0H                                                               
       ++INCLUDE DERDAYPRT                                                      
         EJECT                                                                  
         TITLE '-   DEMO RECORD DATA EXTRACTOR (SUBR01)'                        
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
                                                                                
* At entry,                                                                     
*   RA-->DBLOCK                                                                 
*   RC-->FINED                                                                  
*   GOSUBN = equated sub-routine number.                                        
                                                                                
DEFINE   RSECT                                                                  
         ORG   DEFINE+X'2000'                                                   
         ORG                                                                    
SUBR01Q  EQU   (((*-DEFINE+X'00FF')/X'0100')*X'0100')                           
                                                                                
         ORG   DEFINE+SUBR01Q                                                   
SUBR01   NMOD1 0,**DFN1**                                                       
         LR    RC,R1                                                            
         USING FINED,RC                                                         
                                                                                
         ZIC   R1,GOSUBN                                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         B     R01_00(R1)                                                       
                                                                                
TPNO#    EQU   (R01_01-*)/4+1      TIME PERIOD PROGRAM NUMBER                   
PVNO#    EQU   (R01_02-*)/4+1      PROGRAM AVG PROGRAM NUMBER                   
NDAYS#   EQU   (R01_03-*)/4+1      NUMBER OF DAYS A PROGRAM RAN                 
WEEK#    EQU   (R01_04-*)/4+1      WEEKS                                        
*                                                                               
R01_00   DS    0H                                                               
R01_01   B     TPNO000             TIME PERIOD PROGRAM NUMBER                   
R01_02   B     PVNO000             PROGRAM AVG PROGRAM NUMBER                   
R01_03   B     NDAYS000            NUMBER OF DAYS A PROGRAM RAN                 
R01_04   B     WEEK000             WEEKS                                        
R01#     EQU   (*-R01_00)/4                                                     
         DC    H'0'                                                             
         TITLE '-   DEMO RECORD DATA EXTRACTOR (SUBR01--TPNO#)'                 
*--------------------- TIME PERIOD PROGRAM NUMBER --------------------*         
                                                                                
*                                                                               
TPNO000  DS    0H                                                               
         MVC   0(8,R3),R01SPACE    OUTPUT+0(8) = EBCDIC PROGRAM NUMBER          
                                                                                
*                                                                               
         CLC   MYDBFILE,=C'TP '                                                 
         BNE   TPNO3                                                            
         CLI   DBSELSRC,C'N'                                                    
         BE    TPNO5                                                            
*                                                                               
TPNO3    MVC   0(8,R3),=CL8'N/A'                                                
         B     TPNOX                                                            
                                                                                
*                                                                               
TPNO5    DS    0H                                                               
         MVI   ELCODE,X'21'                                                     
         L     R6,DBAQUART                                                      
         BRAS  RE,FIRSTEL                                                       
         BNE   TPNOX                                                            
*                                                                               
         USING QIELEM,R6                                                        
         CLI   QIELN,4             IS THIS A BACKWARD POINTER?                  
         BNE   TPNO8X               YES, POINT R6 TO ACTUAL ELEMENT             
         ZICM  R0,QIELEM+2,(3)                                                  
         SLL   R0,17                                                            
         SRL   R0,17                                                            
         L     R6,DBAREC                                                        
         AR    R6,R0                                                            
TPNO8X   EQU   *                                                                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,QIPNUM                                                      
         EDIT  (R0),(8,0(R3)),FILL=0                                            
         B     TPNOX                                                            
         DROP  R6                                                               
                                                                                
*                                                                               
TPNOX    DS    0H                                                               
         J     XIT                                                              
         TITLE '-   DEMO RECORD DATA EXTRACTOR (SUBR01--PVNO#)'                 
*--------------------- PROGRAM AVG PROGRAM NUMBER --------------------*         
                                                                                
* TO MAINTAIN SOMEWHAT OF A UNIFORM INTERFACE, THE OUTPUT OF THIS               
*  ROUTINE SHOULD BE THE SAME AS THAT OF THE  TPNO#  ROUTINE                    
                                                                                
PVNO000  DS    0H                                                               
         MVC   0(8,R3),R01SPACE    OUTPUT+0(8) = EBCDIC PROGRAM NUMBER          
                                                                                
*                                                                               
         CLC   MYDBFILE,=C'PAV'                                                 
         BNE   PVNO3                                                            
         CLI   DBSELSRC,C'N'                                                    
         BE    PVNO5                                                            
*                                                                               
PVNO3    MVC   0(8,R3),=CL8'N/A'                                                
         B     PVNOX                                                            
                                                                                
*                                                                               
PVNO5    DS    0H                                                               
         MVI   ELCODE,PHCODEQ                                                   
         L     R6,DBAQUART                                                      
         BRAS  RE,FIRSTEL                                                       
         BNE   PVNOX                                                            
*                                                                               
         USING PHELEM,R6                                                        
         CLI   PHELN,4             IS THIS A BACKWARD POINTER?                  
         BNE   PVNO8X               YES, POINT R6 TO ACTUAL ELEMENT             
         ZICM  R0,PHELEM+2,(3)                                                  
         SLL   R0,17                                                            
         SRL   R0,17                                                            
         L     R6,DBAREC                                                        
         AR    R6,R0                                                            
PVNO8X   EQU   *                                                                
*                                                                               
         CLI   PHREVID,X'FF'       CHECK FOR CORRECT REVISION ID                
         BNE   PVNOX                                                            
*                                                                               
         CLI   PHREV,1                                                          
         BNL   PVNO010                                                          
         B     PVNOX                                                            
                                                                                
*                                                                               
PVNO010  DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,7,PHPNUM3                                                     
         EDIT  (R0),(8,0(R3)),FILL=0                                            
         B     PVNOX                                                            
         DROP  R6                                                               
                                                                                
*                                                                               
PVNOX    DS    0H                                                               
         J     XIT                                                              
         TITLE '-   DEMO RECORD DATA EXTRACTOR (SUBR01--NDAYS#)'                
*-------------------- NUMBER OF DAYS A PROGRAM RAN -------------------*         
                                                                                
NDAYS000 DS    0H                                                               
         MVI   0(R3),0             OUTPUT+0(1) = BINARY # OF DAYS               
         MVC   1(3,R3),=CL16' '    OUTPUT+1(3) = EBCDIC # OF DAYS               
                                                                                
*                                                                               
         CLC   MYDBFILE,=C'PAV'                                                 
         BNE   NDAYSX                                                           
                                                                                
*                                                                               
NDAYS020 DS    0H                  NUMBER OF DAYS FROM PROGRAM AVG RECD         
         USING PHELEM,R7                                                        
         CLI   PHCODE,PHCODEQ       LOOK IN DAY/QH ELEMENT                      
         BNE   NDAYS029                                                         
         CLI   PHREVID,X'FF'        MAKE SURE REVISION ID                       
         BNE   NDAYS029                                                         
         CLI   PHREV,2               IS THE RIGHT ONE                           
         BL    NDAYS029                                                         
         ZIC   R1,PHNDAYS                                                       
         STC   R1,0(R3)             BINARY # OF DAYS                            
         CVD   R1,DUB                                                           
         UNPK  1(3,R3),DUB                                                      
         OI    1+2(R3),X'F0'        EBCDIC # OF DAYS                            
NDAYS029 EQU   *                                                                
         DROP  R7                                                               
                                                                                
*                                                                               
NDAYSX   DS    0H                                                               
         J     XIT                                                              
*                                                                               
WEEK000  CLI   CBLNAD,1                                                         
         BE    WEEK8000                                                         
         MVC   0(5,R3),=CL16' '    PICK OUT WEEK ACTIVITY                       
         LR    R6,R7               MOVE DBAQUART TO R6                          
         CLC   =C'TP',MYDBFILE     TEST TP FILES                                
         BE    WEEK1                                                            
         CLC   MYDBFILE,=C'RDP'    RADIO DAYPART                                
         BE    WEEK1                                                            
         CLC   MYDBFILE,=C'NAD'                                                 
         BE    *+10                TEST FOR NAD                                 
         CLC   MYDBFILE,=C'PAV'                                                 
         BNE   WEEKX                                                            
         USING PHELEM,R6                                                        
         MVC   0(1,R3),PHDWKS      EXTRACT WEEK BITS                            
         B     WEEK2                                                            
         SPACE 1                                                                
         USING QHELEM,R6                                                        
WEEK1    MVC   0(1,R3),QHWKS                                                    
         SPACE 1                                                                
WEEK2    TM    0(R3),X'08'         THEN SHOW ACTIVE WEEKS EDITED                
         BNO   *+8                                                              
         MVI   1(R3),C'1'                                                       
         TM    0(R3),X'04'                                                      
         BNO   *+8                                                              
         MVI   2(R3),C'2'                                                       
         TM    0(R3),X'02'                                                      
         BNO   *+8                                                              
         MVI   3(R3),C'3'                                                       
         TM    0(R3),X'01'                                                      
         BNO   *+8                                                              
         MVI   4(R3),C'4'                                                       
         B     WEEKX                                                            
         DROP  R6                                                               
WEEKX    J     XIT                                                              
*                                                                               
* FOR CABLE NAD, GET WEEKS FROM LONGER FIELD - NTWKS2                           
WEEK8000 MVC   0(7,R3),=CL16' '   MAX 7 CHAR                                    
         MVI   ELCODE,NTCODEQU    R6 POINTS TO NAD INFO AREA IN SPANAD          
         BRAS  RE,GETEL                                                         
         BNE   WEEK8X                                                           
         USING NTELEM,R6                                                        
         MVC   0(1,R3),NTWKS2                                                   
         TM    0(R3),X'80'                                                      
         BNO   *+8                                                              
         MVI   1(R3),C'1'                                                       
         TM    0(R3),X'40'                                                      
         BNO   *+8                                                              
         MVI   2(R3),C'2'                                                       
         TM    0(R3),X'20'                                                      
         BNO   *+8                                                              
         MVI   3(R3),C'3'                                                       
         TM    0(R3),X'10'                                                      
         BNO   *+8                                                              
         MVI   4(R3),C'4'                                                       
         TM    0(R3),X'08'                                                      
         BNO   *+8                                                              
         MVI   5(R3),C'5'          UP TO 5 WEEKS IN A MONTH                     
         B     WEEK8X                                                           
         DROP  R6                                                               
WEEK8X   J     XIT                                                              
         EJECT                                                                  
         TITLE '-   DEMO RECORD DATA EXTRACTOR (SUBR01--LTORG && CONSTA+        
               NTS)'                                                            
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
R01SPACE DS    CL132' '                                                         
         EJECT                                                                  
NETTABL1 DC    H'1000',H'1599',C'NBC '                                          
         DC    H'2000',H'2599',C'ABC '                                          
         DC    H'3000',H'3599',C'CBS '                                          
         DC    H'7000',H'7599',C'FOX '                                          
         DC    X'FF'                                                            
*                                                                               
NETTABL2 DC    H'11',H'13',C'ABC '                                              
         DC    H'21',H'23',C'CBS '                                              
         DC    H'31',H'33',C'NBC '                                              
         DC    H'41',H'43',C'FOX '                                              
         DC    H'51',H'53',C'ALL '                                              
         DC    X'FF'                                                            
                                                                                
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'1000'-SUBR01L+1)                                           
***********************************************************************         
                                                                                
         DROP  RB,RC                                                            
         TITLE '-   DEMO RECORD DATA EXTRACTOR (TABLES)'                        
***********************************************************************         
SUBRTNTB DS    0XL(1+2)                                                         
         DC     AL1(R01#),AL2(SUBR01-DEFINE)                                    
         DC    AL1(0)                                                           
***********************************************************************         
                                                                                
         EJECT                                                                  
NETSUBP  DS    0H                                                               
         DC    AL3(265),CL4'SPOC'                                               
         DC    AL3(462),CL4'TEPR'                                               
         DC    AL3(464),CL4'NFXP'                                               
         DC    AL3(484),CL4'NFRS'                                               
         DC    AL3(1016),CL4'FCBG'                                              
         DC    AL3(1250),CL4'MSNA'                                              
         DC    AL3(1350),CL4'CBKC'                                              
         DC    AL3(1408),CL4'CBKC'                                              
         DC    AL3(1455),CL4'GOLC'                                              
         DC    AL3(1463),CL4'GOLP'                                              
         DC    AL3(1464),CL4'GOLC'                                              
         DC    AL3(1641),CL4'GOLP'                                              
         DC    AL3(1649),CL4'GOLP'                                              
         DC    AL3(1704),CL4'SPOA'                                              
         DC    AL3(1726),CL4'TEPR'                                              
         DC    AL3(1727),CL4'TEPR'                                              
         DC    AL3(1730),CL4'TEOC'                                              
         DC    AL3(1731),CL4'TEOC'                                              
         DC    AL3(1743),CL4'TEOC'                                              
         DC    AL3(1751),CL4'TEOC'                                              
         DC    AL3(1757),CL4'TEOC'                                              
         DC    AL3(2027),CL4'TEOC'                                              
         DC    AL3(2030),CL4'TEOC'                                              
         DC    AL3(2031),CL4'TEOC'                                              
         DC    AL3(2032),CL4'TEOC'                                              
         DC    AL3(2064),CL4'TEPR'                                              
         DC    AL3(2077),CL4'TEPR'                                              
         DC    AL3(2754),CL4'CBKC'                                              
         DC    AL3(2755),CL4'CBKC'                                              
         DC    AL3(2768),CL4'CBKC'                                              
         DC    AL3(2769),CL4'CBKC'                                              
         DC    AL3(2785),CL4'CBXP'                                              
         DC    AL3(2826),CL4'GOLP'                                              
         DC    AL3(2907),CL4'SPOT'                                              
         DC    AL3(2967),CL4'TEPR'                                              
         DC    AL3(2971),CL4'TEPR'                                              
         DC    AL3(3020),CL4'TEOC'                                              
         DC    AL3(3183),CL4'NFPS'                                              
         DC    AL3(4103),CL4'MSIR'                                              
         DC    AL3(4357),CL4'GOLP'                                              
         DC    AL3(4358),CL4'GOLP'                                              
         DC    AL3(4439),CL4'SPOC'                                              
         DC    AL3(4470),CL4'TEOC'                                              
         DC    AL3(5009),CL4'FCBG'                                              
         DC    AL3(5719),CL4'MSCT'                                              
         DC    AL3(5894),CL4'TEPR'                                              
         DC    AL3(6560),CL4'HSRC'                                              
         DC    AL3(6789),CL4'FCBG'                                              
         DC    AL3(7117),CL4'GOLP'                                              
         DC    AL3(7120),CL4'GOLP'                                              
         DC    AL3(7479),CL4'GOLP'                                              
         DC    AL3(7489),CL4'GOLP'                                              
         DC    AL3(9209),CL4'TEOC'                                              
         DC    AL3(9254),CL4'TEOC'                                              
         DC    AL3(9255),CL4'TEOC'                                              
         DC    AL3(9256),CL4'TEOC'                                              
         DC    AL3(9257),CL4'TEOC'                                              
         DC    AL3(10818),CL4'FCBG'                                             
         DC    AL3(11289),CL4'GOLP'                                             
         DC    AL3(11291),CL4'GOLP'                                             
         DC    AL3(11814),CL4'CBKC'                                             
         DC    AL3(13700),CL4'GOLS'                                             
         DC    AL3(13701),CL4'GOLS'                                             
         DC    AL3(14279),CL4'GOLP'                                             
         DC    AL3(14287),CL4'GOLP'                                             
         DC    AL3(14382),CL4'GOLP'                                             
         DC    AL3(14385),CL4'GOLP'                                             
         DC    AL3(14394),CL4'TEPR'                                             
         DC    AL3(18125),CL4'CBXP'                                             
         DC    AL3(18166),CL4'GOLS'                                             
         DC    AL3(18168),CL4'GOLS'                                             
         DC    AL3(18591),CL4'MSIR'                                             
         DC    AL3(18910),CL4'TEPR'                                             
         DC    AL3(18912),CL4'TEPR'                                             
         DC    AL3(19494),CL4'TEPR'                                             
         DC    AL3(19974),CL4'GOLS'                                             
         DC    AL3(19975),CL4'GOLS'                                             
         DC    AL3(20372),CL4'GOLL'                                             
         DC    AL3(20395),CL4'GOLL'                                             
         DC    AL3(21554),CL4'BKOC'                                             
         DC    AL3(22023),CL4'BKXP'                                             
         DC    AL3(22625),CL4'GOOT'                                             
         DC    AL3(22627),CL4'GOOT'                                             
         DC    AL3(23463),CL4'NFCP'                                             
         DC    AL3(23464),CL4'NFXP'                                             
         DC    AL3(23465),CL4'NFCP'                                             
         DC    AL3(23972),CL4'GOOT'                                             
         DC    AL3(23980),CL4'GOOT'                                             
         DC    AL3(23981),CL4'BKXP'                                             
         DC    AL3(23982),CL4'BKRS'                                             
         DC    AL3(24178),CL4'BKRS'                                             
         DC    AL3(24901),CL4'CBOC'                                             
         DC    AL3(24940),CL4'CBKC'                                             
         DC    AL3(24941),CL4'CBKC'                                             
         DC    AL3(24942),CL4'CBKC'                                             
         DC    AL3(24943),CL4'CBKC'                                             
         DC    AL3(24946),CL4'CBKC'                                             
         DC    AL3(24947),CL4'CBKC'                                             
         DC    AL3(24948),CL4'CBKC'                                             
         DC    AL3(24949),CL4'CBKC'                                             
         DC    AL3(24950),CL4'MSOR'                                             
         DC    AL3(24972),CL4'CBKC'                                             
         DC    AL3(24973),CL4'CBKC'                                             
         DC    AL3(24974),CL4'CBKC'                                             
         DC    AL3(24976),CL4'CBKC'                                             
         DC    AL3(24977),CL4'CBKC'                                             
         DC    AL3(24978),CL4'CBKC'                                             
         DC    AL3(25136),CL4'CBKC'                                             
         DC    AL3(25137),CL4'CBKC'                                             
         DC    AL3(25146),CL4'COOT'                                             
         DC    AL3(25147),CL4'CBKC'                                             
         DC    AL3(25148),CL4'CBKC'                                             
         DC    AL3(25151),CL4'CBXP'                                             
         DC    AL3(25154),CL4'CBXP'                                             
         DC    AL3(25366),CL4'CBKC'                                             
         DC    AL3(25413),CL4'CBOC'                                             
         DC    AL3(25480),CL4'BKXP'                                             
         DC    AL3(25482),CL4'BKRS'                                             
         DC    AL3(26141),CL4'BKPO'                                             
         DC    AL3(26149),CL4'BKPO'                                             
         DC    AL3(26157),CL4'BKXP'                                             
         DC    AL3(26158),CL4'BKPO'                                             
         DC    AL3(26162),CL4'BKPO'                                             
         DC    AL3(26163),CL4'BKPO'                                             
         DC    AL3(26238),CL4'BKCH'                                             
         DC    AL3(26248),CL4'BKCH'                                             
         DC    AL3(26250),CL4'BKCH'                                             
         DC    AL3(26288),CL4'BKCH'                                             
         DC    AL3(26471),CL4'BKCH'                                             
         DC    AL3(26690),CL4'TEPR'                                             
         DC    AL3(26692),CL4'TEPR'                                             
         DC    AL3(27564),CL4'GOLC'                                             
         DC    AL3(27605),CL4'GOLP'                                             
         DC    AL3(27615),CL4'GOLC'                                             
         DC    AL3(27617),CL4'GOLP'                                             
         DC    AL3(29725),CL4'FCRS'                                             
         DC    AL3(30057),CL4'BKRS'                                             
         DC    AL3(30632),CL4'CBRS'                                             
         DC    AL3(31641),CL4'CBXP'                                             
         DC    AL3(32335),CL4'GOLS'                                             
         DC    AL3(32352),CL4'GOLS'                                             
         DC    AL3(32427),CL4'HSRC'                                             
         DC    AL3(32428),CL4'HSRC'                                             
         DC    AL3(32624),CL4'HSRC'                                             
         DC    AL3(32625),CL4'HSRC'                                             
         DC    AL3(32982),CL4'HSRC'                                             
         DC    AL3(32983),CL4'HSRC'                                             
         DC    AL3(32984),CL4'BKPO'                                             
         DC    AL3(33251),CL4'BKCH'                                             
         DC    AL3(33470),CL4'TEPR'                                             
         DC    AL3(33508),CL4'TEPR'                                             
         DC    AL3(33545),CL4'TEPR'                                             
         DC    AL3(34547),CL4'NFPS'                                             
         DC    AL3(34665),CL4'NFPS'                                             
         DC    AL3(34666),CL4'MSOR'                                             
         DC    AL3(35223),CL4'FCRS'                                             
         DC    AL3(36472),CL4'BKOC'                                             
         DC    AL3(37671),CL4'GOOT'                                             
         DC    AL3(37672),CL4'GOOT'                                             
         DC    AL3(37856),CL4'NFXP'                                             
         DC    AL3(39348),CL4'SPOT'                                             
         DC    AL3(39987),CL4'MSIR'                                             
         DC    AL3(43867),CL4'FCRS'                                             
         DC    AL3(43869),CL4'FCRS'                                             
         DC    AL3(44375),CL4'TEPR'                                             
         DC    AL3(46070),CL4'FCRS'                                             
         DC    AL3(46071),CL4'FCXP'                                             
         DC    AL3(46072),CL4'FCRS'                                             
         DC    AL3(48798),CL4'GOLP'                                             
         DC    AL3(48815),CL4'GOLP'                                             
         DC    AL3(51280),CL4'GOLS'                                             
         DC    AL3(51478),CL4'GOLS'                                             
         DC    AL3(51647),CL4'GOLO'                                             
         DC    AL3(51673),CL4'GOLO'                                             
         DC    AL3(52007),CL4'BBXP'                                             
         DC    AL3(52564),CL4'MSNA'                                             
         DC    AL3(53387),CL4'FCRS'                                             
         DC    AL3(53617),CL4'NFXP'                                             
         DC    AL3(53619),CL4'NFRS'                                             
         DC    AL3(53621),CL4'NFRS'                                             
         DC    AL3(53624),CL4'NFXP'                                             
         DC    AL3(53667),CL4'NFOT'                                             
         DC    AL3(53668),CL4'NFOT'                                             
         DC    AL3(53769),CL4'NFRS'                                             
         DC    AL3(54814),CL4'GOOT'                                             
         DC    AL3(54816),CL4'GOOT'                                             
         DC    AL3(55338),CL4'FGSK'                                             
         DC    AL3(55661),CL4'NFXP'                                             
         DC    AL3(55662),CL4'NFRS'                                             
         DC    AL3(55737),CL4'NFXP'                                             
         DC    AL3(55924),CL4'NFXP'                                             
         DC    AL3(55925),CL4'NFRS'                                             
         DC    AL3(55926),CL4'NFXP'                                             
         DC    AL3(56079),CL4'NFRS'                                             
         DC    AL3(56080),CL4'NFXP'                                             
         DC    AL3(56081),CL4'NFXP'                                             
         DC    AL3(56239),CL4'BKOC'                                             
         DC    AL3(56370),CL4'NFCP'                                             
         DC    AL3(56373),CL4'NFOT'                                             
         DC    AL3(56374),CL4'NFXP'                                             
         DC    AL3(56378),CL4'FCBG'                                             
         DC    AL3(56485),CL4'NFXP'                                             
         DC    AL3(56486),CL4'NFCP'                                             
         DC    AL3(56487),CL4'NFOT'                                             
         DC    AL3(56488),CL4'NFXP'                                             
         DC    AL3(56489),CL4'NFXP'                                             
         DC    AL3(56490),CL4'NFCP'                                             
         DC    AL3(56491),CL4'NFOT'                                             
         DC    AL3(56492),CL4'NFXP'                                             
         DC    AL3(56691),CL4'NFXP'                                             
         DC    AL3(56692),CL4'NFCP'                                             
         DC    AL3(56693),CL4'NFOT'                                             
         DC    AL3(56694),CL4'NFXP'                                             
         DC    AL3(56695),CL4'CBRS'                                             
         DC    AL3(57218),CL4'NFOT'                                             
         DC    AL3(57342),CL4'SPOC'                                             
         DC    AL3(58593),CL4'CBXP'                                             
         DC    AL3(58979),CL4'CBXP'                                             
         DC    AL3(60307),CL4'GOLP'                                             
         DC    AL3(60308),CL4'GOLP'                                             
         DC    AL3(60444),CL4'SOCC'                                             
         DC    AL3(60474),CL4'GOLC'                                             
         DC    AL3(60658),CL4'GOLP'                                             
         DC    AL3(60734),CL4'GOLP'                                             
         DC    AL3(60929),CL4'GOLS'                                             
         DC    AL3(61002),CL4'GOLS'                                             
         DC    AL3(61006),CL4'TEPR'                                             
         DC    AL3(61118),CL4'GOLL'                                             
         DC    AL3(61124),CL4'GOLL'                                             
         DC    AL3(62325),CL4'GYMN'                                             
         DC    AL3(62329),CL4'GYMN'                                             
         DC    AL3(62418),CL4'FCRS'                                             
         DC    AL3(62605),CL4'NFXP'                                             
         DC    AL3(63826),CL4'BBPO'                                             
         DC    AL3(63997),CL4'BBPO'                                             
         DC    AL3(63999),CL4'BBPO'                                             
         DC    AL3(64000),CL4'BBPO'                                             
         DC    AL3(64075),CL4'BBPO'                                             
         DC    AL3(65211),CL4'GOOT'                                             
         DC    AL3(65225),CL4'GOOT'                                             
         DC    AL3(65353),CL4'NFXP'                                             
         DC    AL3(65558),CL4'NFXP'                                             
         DC    AL3(65852),CL4'NFXP'                                             
         DC    AL3(65853),CL4'NFXP'                                             
         DC    AL3(65911),CL4'FCBG'                                             
         DC    AL3(66028),CL4'FCXP'                                             
         DC    AL3(66141),CL4'NFXP'                                             
         DC    AL3(66142),CL4'NFXP'                                             
         DC    AL3(66237),CL4'NFXP'                                             
         DC    AL3(66346),CL4'CBRS'                                             
         DC    AL3(66350),CL4'MSIR'                                             
         DC    AL3(66476),CL4'NFOT'                                             
         DC    AL3(66479),CL4'CBRS'                                             
         DC    AL3(66667),CL4'BKOC'                                             
         DC    AL3(66906),CL4'SPOT'                                             
         DC    AL3(66927),CL4'MSNA'                                             
         DC    AL3(67514),CL4'CBRS'                                             
         DC    AL3(67521),CL4'GOLP'                                             
         DC    AL3(67522),CL4'GOLP'                                             
         DC    AL3(67738),CL4'CBXP'                                             
         DC    AL3(67776),CL4'CBXP'                                             
         DC    AL3(68056),CL4'CBXP'                                             
         DC    AL3(68733),CL4'GOLP'                                             
         DC    AL3(68738),CL4'GOLP'                                             
         DC    AL3(69475),CL4'BBRS'                                             
         DC    AL3(69615),CL4'BKXP'                                             
         DC    AL3(69659),CL4'BKXP'                                             
         DC    AL3(69689),CL4'SPOT'                                             
         DC    AL3(69733),CL4'BKXP'                                             
         DC    AL3(69811),CL4'GOLP'                                             
         DC    AL3(69821),CL4'GOLP'                                             
         DC    AL3(69822),CL4'BKXP'                                             
         DC    AL3(69827),CL4'BKXP'                                             
         DC    AL3(69860),CL4'BKXP'                                             
         DC    AL3(69872),CL4'OLYM'                                             
         DC    AL3(69873),CL4'OLYM'                                             
         DC    AL3(69875),CL4'OLYM'                                             
         DC    AL3(69876),CL4'OLYM'                                             
         DC    AL3(69877),CL4'OLYM'                                             
         DC    AL3(69878),CL4'OLYM'                                             
         DC    AL3(69879),CL4'OLYM'                                             
         DC    AL3(69880),CL4'OLYM'                                             
         DC    AL3(69881),CL4'OLYM'                                             
         DC    AL3(69882),CL4'OLYM'                                             
         DC    AL3(69883),CL4'OLYM'                                             
         DC    AL3(69884),CL4'OLYM'                                             
         DC    AL3(69885),CL4'OLYM'                                             
         DC    AL3(69886),CL4'OLYM'                                             
         DC    AL3(69887),CL4'OLYM'                                             
         DC    AL3(69888),CL4'OLYM'                                             
         DC    AL3(69889),CL4'OLYM'                                             
         DC    AL3(69890),CL4'OLYM'                                             
         DC    AL3(69893),CL4'OLYM'                                             
         DC    AL3(69895),CL4'OLYM'                                             
         DC    AL3(69896),CL4'OLYM'                                             
         DC    AL3(69897),CL4'OLYM'                                             
         DC    AL3(69899),CL4'OLYM'                                             
         DC    AL3(69900),CL4'OLYM'                                             
         DC    AL3(69911),CL4'OLYM'                                             
         DC    AL3(69912),CL4'OLYM'                                             
         DC    AL3(69913),CL4'OLYM'                                             
         DC    AL3(69914),CL4'OLYM'                                             
         DC    AL3(69915),CL4'OLYM'                                             
         DC    AL3(69916),CL4'OLYM'                                             
         DC    AL3(69918),CL4'OLYM'                                             
         DC    AL3(69919),CL4'OLYM'                                             
         DC    AL3(69920),CL4'OLYM'                                             
         DC    AL3(69921),CL4'OLYM'                                             
         DC    AL3(69922),CL4'OLYM'                                             
         DC    AL3(69923),CL4'OLYM'                                             
         DC    AL3(69924),CL4'OLYM'                                             
         DC    AL3(69925),CL4'OLYM'                                             
         DC    AL3(69926),CL4'OLYM'                                             
         DC    AL3(69927),CL4'OLYM'                                             
         DC    AL3(69928),CL4'OLYM'                                             
         DC    AL3(69929),CL4'OLYM'                                             
         DC    AL3(69930),CL4'OLYM'                                             
         DC    AL3(69931),CL4'OLYM'                                             
         DC    AL3(69932),CL4'OLYM'                                             
         DC    AL3(70322),CL4'BBOT'                                             
         DC    AL3(70801),CL4'GOLC'                                             
         DC    AL3(71417),CL4'FCXP'                                             
         DC    AL3(71526),CL4'GOLO'                                             
         DC    AL3(71527),CL4'GOLO'                                             
         DC    AL3(71531),CL4'TENN '                                            
         DC    AL3(71740),CL4'TEPR'                                             
         DC    AL3(71744),CL4'TEPR'                                             
         DC    AL3(71930),CL4'TEPR'                                             
         DC    AL3(71932),CL4'TEPR'                                             
         DC    AL3(71956),CL4'NFXP'                                             
         DC    AL3(72388),CL4'BBPO'                                             
         DC    AL3(72445),CL4'BBPO'                                             
         DC    AL3(72446),CL4'BBPO'                                             
         DC    AL3(72447),CL4'BBPO'                                             
         DC    AL3(72515),CL4'BBPO'                                             
         DC    AL3(72605),CL4'BBPO'                                             
         DC    AL3(72661),CL4'BBPO'                                             
         DC    AL3(72662),CL4'BBPO'                                             
         DC    AL3(72670),CL4'BBPO'                                             
         DC    AL3(72794),CL4'BBPO'                                             
         DC    AL3(72796),CL4'BBPO'                                             
         DC    AL3(72797),CL4'BBXP'                                             
         DC    AL3(72798),CL4'BBPO'                                             
         DC    AL3(72799),CL4'BBXP'                                             
         DC    AL3(72976),CL4'BBXP'                                             
         DC    AL3(72978),CL4'BBWS'                                             
         DC    AL3(72982),CL4'BBWS'                                             
         DC    AL3(73033),CL4'BBWS'                                             
         DC    AL3(73145),CL4'BBWS'                                             
         DC    AL3(73207),CL4'BBWS'                                             
         DC    AL3(73599),CL4'GOLO'                                             
         DC    AL3(73600),CL4'GOLO'                                             
         DC    AL3(74254),CL4'FCCC'                                             
         DC    AL3(74259),CL4'FCCC'                                             
         DC    AL3(74904),CL4'FCXP'                                             
         DC    AL3(74936),CL4'CBWM'                                             
         DC    AL3(75624),CL4'BKOC'                                             
         DC    AL3(76788),CL4'CBXP'                                             
         DC    AL3(77020),CL4'CBXP'                                             
         DC    AL3(77341),CL4'BKRS'                                             
         DC    AL3(77410),CL4'GOLP'                                             
         DC    AL3(77413),CL4'GOLP'                                             
         DC    AL3(78451),CL4'BKXP'                                             
         DC    AL3(78460),CL4'BKXP'                                             
         DC    AL3(79034),CL4'WNBA'                                             
         DC    AL3(79035),CL4'WNBA'                                             
         DC    AL3(79543),CL4'BBOT'                                             
         DC    AL3(79585),CL4'MSNT'                                             
         DC    AL3(79587),CL4'MSNB'                                             
         DC    AL3(80905),CL4'FCXP'                                             
         DC    AL3(80908),CL4'FCXP'                                             
         DC    AL3(80928),CL4'GOLP'                                             
         DC    AL3(81158),CL4'NFXP'                                             
         DC    AL3(81600),CL4'FCXP'                                             
         DC    AL3(82253),CL4'BBPO'                                             
         DC    AL3(83491),CL4'FCXP'                                             
         DC    AL3(83806),CL4'NFXP'                                             
         DC    AL3(83807),CL4'NFXP'                                             
         DC    AL3(83819),CL4'FCRS'                                             
         DC    AL3(84483),CL4'FCBG'                                             
         DC    AL3(84524),CL4'CBRS'                                             
         DC    AL3(84626),CL4'NFXP'                                             
         DC    AL3(84627),CL4'NFXP'                                             
         DC    AL3(84752),CL4'FCXP'                                             
         DC    AL3(84753),CL4'FCXP'                                             
         DC    AL3(84894),CL4'GOLC'                                             
         DC    AL3(84945),CL4'FGSK'                                             
         DC    AL3(85016),CL4'CBRS'                                             
         DC    AL3(85745),CL4'MSNA'                                             
         DC    AL3(86024),CL4'MSNB'                                             
         DC    AL3(86529),CL4'NFXP'                                             
         DC    AL3(86638),CL4'CBCC'                                             
         DC    AL3(86639),CL4'CBCC'                                             
         DC    AL3(86640),CL4'CBCC'                                             
         DC    AL3(86786),CL4'WNSP'                                             
         DC    AL3(86812),CL4'CBXP'                                             
         DC    AL3(87391),CL4'FGSK'                                             
         DC    AL3(87418),CL4'SPOT'                                             
         DC    AL3(87420),CL4'SPOT'                                             
         DC    AL3(87580),CL4'SPOC'                                             
         DC    AL3(87849),CL4'SPOC'                                             
         DC    AL3(88054),CL4'MSNA'                                             
         DC    AL3(88277),CL4'COOT'                                             
         DC    AL3(89115),CL4'CBBW'                                             
         DC    AL3(89117),CL4'BKXP'                                             
         DC    AL3(89408),CL4'CBBW'                                             
         DC    AL3(89410),CL4'COOT'                                             
         DC    AL3(89427),CL4'BKOC'                                             
         DC    AL3(90098),CL4'MSNA'                                             
         DC    AL3(90278),CL4'MSCT'                                             
         DC    AL3(90395),CL4'SPOA'                                             
         DC    AL3(90929),CL4'NFPS'                                             
         DC    AL3(90931),CL4'SOCC'                                             
         DC    AL3(91081),CL4'MSCT'                                             
         DC    AL3(91357),CL4'NFXP'                                             
         DC    AL3(91358),CL4'NFXP'                                             
         DC    AL3(91463),CL4'WNBZ'                                             
         DC    AL3(91760),CL4'NFXP'                                             
         DC    AL3(91761),CL4'NFXP'                                             
         DC    AL3(91763),CL4'SPOT'                                             
         DC    AL3(91899),CL4'TEPR'                                             
         DC    AL3(92153),CL4'NFRS'                                             
         DC    AL3(92154),CL4'NFRS'                                             
         DC    AL3(92156),CL4'NFXP'                                             
         DC    AL3(92157),CL4'NFXP'                                             
         DC    AL3(92162),CL4'NFXP'                                             
         DC    AL3(92429),CL4'FCRS'                                             
         DC    AL3(92443),CL4'NFRS'                                             
         DC    AL3(92444),CL4'NFXP'                                             
         DC    AL3(92445),CL4'NFXP'                                             
         DC    AL3(92446),CL4'NFRS'                                             
         DC    AL3(92862),CL4'FGSK'                                             
         DC    AL3(92966),CL4'MSCT'                                             
         DC    AL3(93531),CL4'BBXP'                                             
         DC    AL3(93636),CL4'BBXP'                                             
         DC    AL3(93637),CL4'BBXP'                                             
         DC    AL3(93638),CL4'BBXP'                                             
         DC    AL3(93909),CL4'NFXP'                                             
         DC    AL3(94278),CL4'NFXP'                                             
         DC    AL3(94479),CL4'GOLP'                                             
         DC    AL3(94480),CL4'GOLP'                                             
         DC    AL3(95139),CL4'NFXP'                                             
         DC    AL3(95140),CL4'NFRS'                                             
         DC    AL3(95141),CL4'NFXP'                                             
         DC    AL3(95142),CL4'NFXP'                                             
         DC    AL3(95341),CL4'FCOC'                                             
         DC    AL3(95342),CL4'NFXP'                                             
         DC    AL3(95354),CL4'FGSK'                                             
         DC    AL3(95589),CL4'SPOA'                                             
         DC    AL3(95749),CL4'NFXP'                                             
         DC    AL3(95750),CL4'NFXP'                                             
         DC    AL3(95751),CL4'NFXP'                                             
         DC    AL3(95810),CL4'NFXP'                                             
         DC    AL3(95924),CL4'NFXP'                                             
         DC    AL3(95926),CL4'NFXP'                                             
         DC    AL3(95927),CL4'NFRS'                                             
         DC    AL3(95929),CL4'NFXP'                                             
         DC    AL3(96001),CL4'CBXP'                                             
         DC    AL3(96038),CL4'FCBG'                                             
         DC    AL3(96039),CL4'FCXP'                                             
         DC    AL3(96131),CL4'NFXP'                                             
         DC    AL3(96132),CL4'NFXP'                                             
         DC    AL3(96134),CL4'FCXP'                                             
         DC    AL3(96135),CL4'FCBG'                                             
         DC    AL3(96136),CL4'NFCP'                                             
         DC    AL3(96157),CL4'NFXP'                                             
         DC    AL3(96158),CL4'NFXP'                                             
         DC    AL3(96159),CL4'NFXP'                                             
         DC    AL3(96160),CL4'NFXP'                                             
         DC    AL3(96262),CL4'NFCP'                                             
         DC    AL3(96263),CL4'NFCP'                                             
         DC    AL3(96264),CL4'NFXP'                                             
         DC    AL3(96321),CL4'NFXP'                                             
         DC    AL3(96322),CL4'NFXP'                                             
         DC    AL3(96323),CL4'NFXP'                                             
         DC    AL3(96324),CL4'NFXP'                                             
         DC    AL3(96325),CL4'NFXP'                                             
         DC    AL3(96326),CL4'NFXP'                                             
         DC    AL3(96328),CL4'GOLC'                                             
         DC    AL3(96384),CL4'NFCP'                                             
         DC    AL3(96449),CL4'NFXP'                                             
         DC    AL3(96460),CL4'NFXP'                                             
         DC    AL3(96461),CL4'NFXP'                                             
         DC    AL3(96641),CL4'FGSK'                                             
         DC    AL3(96649),CL4'HSRC'                                             
         DC    AL3(96767),CL4'GOLC'                                             
         DC    AL3(96773),CL4'GOLC'                                             
         DC    AL3(96780),CL4'GOLP'                                             
         DC    AL3(96781),CL4'GOLP'                                             
         DC    AL3(96813),CL4'GOLC'                                             
         DC    AL3(96814),CL4'GOLC'                                             
         DC    AL3(96898),CL4'MSNA'                                             
         DC    AL3(96899),CL4'SPOA'                                             
         DC    AL3(97067),CL4'GOLC'                                             
         DC    AL3(97071),CL4'GOLC'                                             
         DC    AL3(97072),CL4'GOLC'                                             
         DC    AL3(97088),CL4'FGSK'                                             
         DC    AL3(97090),CL4'FGSK'                                             
         DC    AL3(97093),CL4'GOLP'                                             
         DC    AL3(97094),CL4'GOLP'                                             
         DC    AL3(97185),CL4'BKRS'                                             
         DC    AL3(97215),CL4'GOLC'                                             
         DC    AL3(97216),CL4'GOLC'                                             
         DC    AL3(97221),CL4'FGSK'                                             
         DC    AL3(97229),CL4'BKXP'                                             
         DC    AL3(97505),CL4'GOLP'                                             
         DC    AL3(97507),CL4'GOLP'                                             
         DC    AL3(97522),CL4'HSRC'                                             
         DC    AL3(97529),CL4'FGSK'                                             
         DC    AL3(97545),CL4'CBXP'                                             
         DC    AL3(97546),CL4'CBRS'                                             
         DC    AL3(97547),CL4'FGSK'                                             
         DC    AL3(97574),CL4'CBXP'                                             
         DC    AL3(97735),CL4'GOLC'                                             
         DC    AL3(97776),CL4'CBXP'                                             
         DC    AL3(97777),CL4'CBXP'                                             
         DC    AL3(97778),CL4'CBXP'                                             
         DC    AL3(97973),CL4'GOLP'                                             
         DC    AL3(97974),CL4'GOLP'                                             
         DC    AL3(98047),CL4'TEPR'                                             
         DC    AL3(98048),CL4'TEPR'                                             
         DC    AL3(98049),CL4'HSRC'                                             
         DC    AL3(98058),CL4'SPOT'                                             
         DC    AL3(98152),CL4'GOLO'                                             
         DC    AL3(98280),CL4'GOLC'                                             
         DC    AL3(98451),CL4'TEPR'                                             
         DC    AL3(98633),CL4'GOLP'                                             
         DC    AL3(98634),CL4'GOLP'                                             
         DC    AL3(98656),CL4'GOLP'                                             
         DC    AL3(98657),CL4'GOLP'                                             
         DC    AL3(98725),CL4'BKPO'                                             
         DC    AL3(98726),CL4'BKPO'                                             
         DC    AL3(98727),CL4'BKPO'                                             
         DC    AL3(98728),CL4'BKPO'                                             
         DC    AL3(98729),CL4'BKPO'                                             
         DC    AL3(98730),CL4'BKPO'                                             
         DC    AL3(98906),CL4'GOLP'                                             
         DC    AL3(98913),CL4'GOLP'                                             
         DC    AL3(99116),CL4'GOLL'                                             
         DC    AL3(99118),CL4'GOLL'                                             
         DC    AL3(99264),CL4'MSOC'                                             
         DC    AL3(99266),CL4'MSIR'                                             
         DC    AL3(99554),CL4'NFOT'                                             
         DC    AL3(99555),CL4'GOLC'                                             
         DC    AL3(99556),CL4'GOLC'                                             
         DC    AL3(99557),CL4'GOLP'                                             
         DC    AL3(99558),CL4'GOLP'                                             
         DC    AL3(99583),CL4'MSCT'                                             
         DC    AL3(99660),CL4'GOLP'                                             
         DC    AL3(99733),CL4'MSOC'                                             
         DC    AL3(99734),CL4'MSNA'                                             
         DC    AL3(99889),CL4'GOLP'                                             
         DC    AL3(99930),CL4'GOLP'                                             
         DC    AL3(100000),CL4'GOLL'                                            
         DC    AL3(100034),CL4'GOLL'                                            
         DC    AL3(100080),CL4'BKXP'                                            
         DC    AL3(100221),CL4'MSOC'                                            
         DC    AL3(100362),CL4'NFOT'                                            
         DC    AL3(100370),CL4'NFOT'                                            
         DC    AL3(100574),CL4'TENA'                                            
         DC    AL3(100791),CL4'MSOR'                                            
         DC    AL3(100831),CL4'GOLP'                                            
         DC    AL3(100832),CL4'GOLP'                                            
         DC    AL3(101168),CL4'BBOT'                                            
         DC    AL3(101327),CL4'NFXP'                                            
         DC    AL3(101334),CL4'NFXP'                                            
         DC    AL3(101651),CL4'WRST'                                            
         DC    AL3(101817),CL4'GOLC'                                            
         DC    AL3(101875),CL4'GOLC'                                            
         DC    AL3(102008),CL4'WNOC'                                            
         DC    AL3(102010),CL4'WNBC'                                            
         DC    AL3(102014),CL4'MSCT'                                            
         DC    AL3(102122),CL4'NFOC'                                            
         DC    AL3(102178),CL4'WNOC'                                            
         DC    AL3(102329),CL4'SPOC'                                            
         DC    AL3(102388),CL4'NFOT'                                            
         DC    AL3(102510),CL4'FCRS'                                            
         DC    AL3(102512),CL4'SPOA'                                            
         DC    AL3(102513),CL4'SPOT'                                            
         DC    AL3(102866),CL4'FCXP'                                            
         DC    AL3(103010),CL4'ETGA'                                            
         DC    AL3(103098),CL4'GOLP'                                            
         DC    AL3(103292),CL4'FGSK'                                            
         DC    AL3(103615),CL4'GOLC'                                            
         DC    AL3(103616),CL4'GOLP'                                            
         DC    AL3(103619),CL4'FCRS'                                            
         DC    AL3(103630),CL4'FGSK'                                            
         DC    AL3(104262),CL4'MSNB'                                            
         DC    AL3(104263),CL4'MSNA'                                            
         DC    AL3(104784),CL4'SPOA'                                            
         DC    AL3(104808),CL4'FGSK'                                            
         DC    AL3(105027),CL4'SPOT'                                            
         DC    AL3(105028),CL4'SPOA'                                            
         DC    AL3(105042),CL4'FGSK'                                            
         DC    AL3(105460),CL4'FGSK'                                            
         DC    AL3(105666),CL4'BKRS'                                            
         DC    AL3(105680),CL4'GOOT'                                            
         DC    AL3(105681),CL4'GOOT'                                            
         DC    AL3(105722),CL4'NFOC'                                            
         DC    AL3(105932),CL4'SPOA'                                            
         DC    AL3(106004),CL4'FCXP'                                            
         DC    AL3(106005),CL4'FCXP'                                            
         DC    AL3(106015),CL4'FCXP'                                            
         DC    AL3(106023),CL4'FCBG'                                            
         DC    AL3(106024),CL4'NFOC'                                            
         DC    AL3(106063),CL4'GOLP'                                            
         DC    AL3(106065),CL4'FCXP'                                            
         DC    AL3(106090),CL4'FCOC'                                            
         DC    AL3(106096),CL4'NFOT'                                            
         DC    AL3(106145),CL4'CBWM'                                            
         DC    AL3(106146),CL4'CBWM'                                            
         DC    AL3(106149),CL4'FGSK'                                            
         DC    AL3(106174),CL4'SPOC'                                            
         DC    AL3(106234),CL4'NFXP'                                            
         DC    AL3(106235),CL4'SPOC'                                            
         DC    AL3(106346),CL4'FGSK'                                            
         DC    AL3(106347),CL4'FGSK'                                            
         DC    AL3(106387),CL4'SPOA'                                            
         DC    AL3(106389),CL4'SPOA'                                            
         DC    AL3(106392),CL4'CBRS'                                            
         DC    AL3(106393),CL4'CBXP'                                            
         DC    AL3(106407),CL4'BKXP'                                            
         DC    AL3(106451),CL4'NFXP'                                            
         DC    AL3(106601),CL4'NFXP'                                            
         DC    AL3(106602),CL4'NFXP'                                            
         DC    AL3(106603),CL4'NFXP'                                            
         DC    AL3(106604),CL4'NFXP'                                            
         DC    AL3(106605),CL4'NFXP'                                            
         DC    AL3(106606),CL4'NFXP'                                            
         DC    AL3(106607),CL4'NFXP'                                            
         DC    AL3(106608),CL4'NFSB'                                            
         DC    AL3(106609),CL4'NFXP'                                            
         DC    AL3(106610),CL4'NFXP'                                            
         DC    AL3(106616),CL4'NFXP'                                            
         DC    AL3(106666),CL4'COOT'                                            
         DC    AL3(106667),CL4'NFOC'                                            
         DC    AL3(106763),CL4'GOLP'                                            
         DC    AL3(106764),CL4'GOLP'                                            
         DC    AL3(106857),CL4'ETGA'                                            
         DC    AL3(106859),CL4'SPOC'                                            
         DC    AL3(106873),CL4'ETGA'                                            
         DC    AL3(106875),CL4'NHOT'                                            
         DC    AL3(106975),CL4'BKOC'                                            
         DC    AL3(106998),CL4'NFXP'                                            
         DC    AL3(107042),CL4'GOLP'                                            
         DC    AL3(107077),CL4'SPOT'                                            
         DC    AL3(107078),CL4'ETGA'                                            
         DC    AL3(107079),CL4'SPOA'                                            
         DC    AL3(107099),CL4'WNSP'                                            
         DC    AL3(107102),CL4'CBRS'                                            
         DC    AL3(107105),CL4'CBXP'                                            
         DC    AL3(107171),CL4'ETGA'                                            
         DC    AL3(107172),CL4'BKOT'                                            
         DC    AL3(107182),CL4'CBRS'                                            
         DC    AL3(107183),CL4'CBXP'                                            
         DC    AL3(107285),CL4'ETGA'                                            
         DC    AL3(107289),CL4'GOLS'                                            
         DC    AL3(107295),CL4'CBRS'                                            
         DC    AL3(107296),CL4'CBXP'                                            
         DC    AL3(107334),CL4'MSTT'                                            
         DC    AL3(107387),CL4'WNSP'                                            
         DC    AL3(107437),CL4'FGSK'                                            
         DC    AL3(107438),CL4'GOLP'                                            
         DC    AL3(107457),CL4'GOLP'                                            
         DC    AL3(107564),CL4'WNSP'                                            
         DC    AL3(107659),CL4'MSNA'                                            
         DC    AL3(107688),CL4'SPOA'                                            
         DC    AL3(107702),CL4'BKXP'                                            
         DC    AL3(107732),CL4'MSNB'                                            
         DC    AL3(107733),CL4'FGSK'                                            
         DC    AL3(107879),CL4'MSNA'                                            
         DC    AL3(107910),CL4'CBXP'                                            
         DC    AL3(107923),CL4'NHRS'                                            
         DC    AL3(107926),CL4'CBXP'                                            
         DC    AL3(107928),CL4'CBXP'                                            
         DC    AL3(107929),CL4'CBXP'                                            
         DC    AL3(107930),CL4'CBXP'                                            
         DC    AL3(107931),CL4'CBXP'                                            
         DC    AL3(107941),CL4'HSRC'                                            
         DC    AL3(107976),CL4'CBXP'                                            
         DC    AL3(108082),CL4'TEPR'                                            
         DC    AL3(108083),CL4'TEPR'                                            
         DC    AL3(108093),CL4'SOCC'                                            
         DC    AL3(108094),CL4'HSRC'                                            
         DC    AL3(108095),CL4'GOLL'                                            
         DC    AL3(108110),CL4'NHRS'                                            
         DC    AL3(108111),CL4'GOLL'                                            
         DC    AL3(108179),CL4'NHRS'                                            
         DC    AL3(108180),CL4'GOLS'                                            
         DC    AL3(108188),CL4'CBXP'                                            
         DC    AL3(108262),CL4'CBXP'                                            
         DC    AL3(108266),CL4'FGSK'                                            
         DC    AL3(108267),CL4'GOLS'                                            
         DC    AL3(108271),CL4'GOLP'                                            
         DC    AL3(108275),CL4'TEPR'                                            
         DC    AL3(108276),CL4'MSOT'                                            
         DC    AL3(108293),CL4'TEPR'                                            
         DC    AL3(108375),CL4'CBOC'                                            
         DC    AL3(108376),CL4'CBXP'                                            
         DC    AL3(108377),CL4'MSOC'                                            
         DC    AL3(108378),CL4'MSNA'                                            
         DC    AL3(108443),CL4'FGSK'                                            
         DC    AL3(108467),CL4'MSOT'                                            
         DC    AL3(108468),CL4'NHRS'                                            
         DC    AL3(108577),CL4'SPOA'                                            
         DC    AL3(108578),CL4'SPOA'                                            
         DC    AL3(108598),CL4'NHCP'                                            
         DC    AL3(108599),CL4'HSRC'                                            
         DC    AL3(108753),CL4'MSIR'                                            
         DC    AL3(108756),CL4'NHCP'                                            
         DC    AL3(108897),CL4'NHCP'                                            
         DC    AL3(108898),CL4'MSMC'                                            
         DC    AL3(108899),CL4'MSNA'                                            
         DC    AL3(109054),CL4'WRST'                                            
         DC    AL3(109074),CL4'NHCP'                                            
         DC    AL3(109427),CL4'FGSK'                                            
         DC    AL3(109428),CL4'MSCT'                                            
         DC    AL3(109429),CL4'NHCP'                                            
         DC    AL3(109507),CL4'GOLL'                                            
         DC    AL3(109508),CL4'GOLL'                                            
         DC    AL3(109513),CL4'HSRC'                                            
         DC    AL3(109558),CL4'NHCP'                                            
         DC    AL3(109717),CL4'NHOC'                                            
         DC    AL3(109718),CL4'SPOT'                                            
         DC    AL3(109720),CL4'SPOT'                                            
         DC    AL3(109846),CL4'BBOC'                                            
         DC    AL3(109864),CL4'SPOT'                                            
         DC    AL3(109877),CL4'BKPO'                                            
         DC    AL3(109912),CL4'WNBA'                                            
         DC    AL3(109918),CL4'GOLS'                                            
         DC    AL3(109919),CL4'SOCC'                                            
         DC    AL3(109920),CL4'NHCP'                                            
         DC    AL3(109921),CL4'GOLS'                                            
         DC    AL3(109943),CL4'MSOT'                                            
         DC    AL3(109947),CL4'GOOT'                                            
         DC    AL3(109955),CL4'GOLP'                                            
         DC    AL3(110046),CL4'NHCP'                                            
         DC    AL3(110048),CL4'ETGA'                                            
         DC    AL3(110051),CL4'BKPO'                                            
         DC    AL3(110052),CL4'BKPO'                                            
         DC    AL3(110064),CL4'SPOT'                                            
         DC    AL3(110129),CL4'BOXX'                                            
         DC    AL3(110255),CL4'MSOT'                                            
         DC    AL3(110257),CL4'NHCP'                                            
         DC    AL3(110324),CL4'SPOT'                                            
         DC    AL3(110332),CL4'TEPR'                                            
         DC    AL3(110343),CL4'NHCP'                                            
         DC    AL3(110344),CL4'BOXX'                                            
         DC    AL3(110345),CL4'MSIR'                                            
         DC    AL3(110348),CL4'GOLC'                                            
         DC    AL3(110493),CL4'SPOT'                                            
         DC    AL3(110542),CL4'GOLP'                                            
         DC    AL3(110593),CL4'GOLP'                                            
         DC    AL3(110725),CL4'SPOT'                                            
         DC    AL3(110771),CL4'NFOT'                                            
         DC    AL3(110804),CL4'BIKE'                                            
         DC    AL3(110805),CL4'GOLP'                                            
         DC    AL3(110810),CL4'GOLP'                                            
         DC    AL3(110994),CL4'BIKE'                                            
         DC    AL3(110995),CL4'GOLP'                                            
         DC    AL3(110996),CL4'GOLP'                                            
         DC    AL3(111063),CL4'SPOT'                                            
         DC    AL3(111122),CL4'SPOT'                                            
         DC    AL3(111132),CL4'SPOT'                                            
         DC    AL3(111133),CL4'SPOT'                                            
         DC    AL3(111137),CL4'HSRC'                                            
         DC    AL3(111142),CL4'MSNT'                                            
         DC    AL3(111143),CL4'BIKE'                                            
         DC    AL3(111323),CL4'GOLP'                                            
         DC    AL3(111324),CL4'GOLC'                                            
         DC    AL3(111325),CL4'BIKE'                                            
         DC    AL3(111326),CL4'GOLP'                                            
         DC    AL3(111327),CL4'GOLP'                                            
         DC    AL3(111328),CL4'BBXP'                                            
         DC    AL3(111335),CL4'MSNT'                                            
         DC    AL3(111410),CL4'SPOT'                                            
         DC    AL3(111451),CL4'SPOT'                                            
         DC    AL3(111455),CL4'SPOT'                                            
         DC    AL3(111456),CL4'SPOT'                                            
         DC    AL3(111457),CL4'SPOT'                                            
         DC    AL3(111563),CL4'BOXX'                                            
         DC    AL3(111565),CL4'MSCT'                                            
         DC    AL3(111572),CL4'SPOT'                                            
         DC    AL3(111718),CL4'SPOT'                                            
         DC    AL3(111757),CL4'SOCC'                                            
         DC    AL3(111832),CL4'OLYM'                                            
         DC    AL3(111853),CL4'SPOT'                                            
         DC    AL3(111854),CL4'GOLP'                                            
         DC    AL3(111859),CL4'WNOC'                                            
         DC    AL3(111892),CL4'SOCC'                                            
         DC    AL3(111893),CL4'SOCC'                                            
         DC    AL3(111905),CL4'HSRC'                                            
         DC    AL3(111906),CL4'TEPR'                                            
         DC    AL3(111907),CL4'GOLP'                                            
         DC    AL3(111936),CL4'GOLC'                                            
         DC    AL3(111947),CL4'SPOT'                                            
         DC    AL3(111969),CL4'NFOC'                                            
         DC    AL3(111970),CL4'ETGA'                                            
         DC    AL3(111971),CL4'SOCC'                                            
         DC    AL3(111972),CL4'NFOT'                                            
         DC    AL3(111977),CL4'NFXP'                                            
         DC    AL3(111978),CL4'NFXP'                                            
         DC    AL3(111981),CL4'SPOT'                                            
         DC    AL3(111988),CL4'SPOC'                                            
         DC    AL3(111989),CL4'SPOC'                                            
         DC    AL3(111995),CL4'NFPS'                                            
         DC    AL3(112249),CL4'NFPS'                                            
         DC    AL3(112251),CL4'MSCT'                                            
         DC    AL3(112320),CL4'GOLS'                                            
         DC    AL3(112322),CL4'NFPS'                                            
         DC    AL3(112330),CL4'SPOT'                                            
         DC    AL3(112331),CL4'FCOC'                                            
         DC    AL3(112332),CL4'ETGA'                                            
         DC    AL3(112333),CL4'SPOA'                                            
         DC    AL3(112337),CL4'NFXP'                                            
         DC    AL3(112339),CL4'NFXP'                                            
         DC    AL3(112474),CL4'NFOA'                                            
         DC    AL3(112508),CL4'GOOT'                                            
         DC    AL3(112514),CL4'SOCC'                                            
         DC    AL3(112536),CL4'SPOT'                                            
         DC    AL3(112549),CL4'SPOT'                                            
         DC    AL3(112617),CL4'GOLO'                                            
         DC    AL3(112788),CL4'SPOT'                                            
         DC    AL3(112830),CL4'GOOT'                                            
         DC    AL3(112838),CL4'SPOA'                                            
         DC    AL3(112839),CL4'MSOR'                                            
         DC    AL3(112909),CL4'OLYM'                                            
         DC    AL3(112912),CL4'OLYM'                                            
         DC    AL3(112913),CL4'OLYM'                                            
         DC    AL3(113022),CL4'MSOR'                                            
         DC    AL3(113242),CL4'BOXX'                                            
         DC    AL3(114037),CL4'SOCC'                                            
         DC    AL3(114038),CL4'MSIR'                                            
         DC    AL3(114042),CL4'BBXP'                                            
         DC    AL3(114182),CL4'GOLC'                                            
         DC    AL3(114194),CL4'FCXP'                                            
         DC    AL3(114269),CL4'BBXP'                                            
         DC    AL3(114373),CL4'FGSK'                                            
         DC    AL3(114490),CL4'GOLP'                                            
         DC    AL3(114491),CL4'GOLP'                                            
         DC    AL3(114603),CL4'BBXP'                                            
         DC    AL3(114637),CL4'BBXP'                                            
         DC    AL3(114655),CL4'GOLP'                                            
         DC    AL3(114664),CL4'FGSK'                                            
         DC    AL3(114669),CL4'GOLP'                                            
         DC    AL3(114675),CL4'FGSK'                                            
         DC    AL3(114878),CL4'GOLP'                                            
         DC    AL3(114879),CL4'GOLP'                                            
         DC    AL3(115046),CL4'FGSK'                                            
         DC    AL3(115047),CL4'GOLL'                                            
         DC    AL3(115188),CL4'WRST'                                            
         DC    AL3(115342),CL4'SPOT'                                            
         DC    AL3(115401),CL4'GOOT'                                            
         DC    AL3(115523),CL4'SPOC'                                            
         DC    AL3(115526),CL4'GOOT'                                            
         DC    AL3(115530),CL4'FGSK'                                            
         DC    AL3(115682),CL4'FCCC'                                            
         DC    AL3(115827),CL4'FCXP'                                            
         DC    AL3(115833),CL4'GOOT'                                            
         DC    AL3(115835),CL4'SPOC'                                            
         DC    AL3(115838),CL4'GOOT'                                            
         DC    AL3(115839),CL4'FGSK'                                            
         DC    AL3(115985),CL4'FGSK'                                            
         DC    AL3(115986),CL4'GOOT'                                            
         DC    AL3(115990),CL4'FGSK'                                            
         DC    AL3(115991),CL4'GOOT'                                            
         DC    AL3(116009),CL4'FCOC'                                            
         DC    AL3(116160),CL4'SPOA'                                            
         DC    AL3(116178),CL4'FGSK'                                            
         DC    AL3(116191),CL4'FGSK'                                            
         DC    AL3(116203),CL4'CBXP'                                            
         DC    AL3(116245),CL4'SPOT'                                            
         DC    AL3(116246),CL4'GOLC'                                            
         DC    AL3(116253),CL4'NFXP'                                            
         DC    AL3(116275),CL4'BKRS'                                            
         DC    AL3(116486),CL4'NFXP'                                            
         DC    AL3(116570),CL4'FCBG'                                            
         DC    AL3(116666),CL4'NFXP'                                            
         DC    AL3(116802),CL4'GOLS'                                            
         DC    AL3(116803),CL4'NFXP'                                            
         DC    AL3(116809),CL4'GOLP'                                            
         DC    AL3(116810),CL4'GOLP'                                            
         DC    AL3(116811),CL4'NFXP'                                            
         DC    AL3(116812),CL4'NFXP'                                            
         DC    AL3(116813),CL4'NFXP'                                            
         DC    AL3(116888),CL4'GOLC'                                            
         DC    AL3(116889),CL4'GOLC'                                            
         DC    AL3(116911),CL4'GOLS'                                            
         DC    AL3(116932),CL4'GOLO'                                            
         DC    AL3(116940),CL4'CBRS'                                            
         DC    AL3(116942),CL4'CBXP'                                            
         DC    AL3(116943),CL4'CBRS'                                            
         DC    AL3(116944),CL4'CBXP'                                            
         DC    AL3(116947),CL4'CBRS'                                            
         DC    AL3(116949),CL4'CBXP'                                            
         DC    AL3(116989),CL4'GOLC'                                            
         DC    AL3(117038),CL4'GOLC'                                            
         DC    AL3(117039),CL4'NFOC'                                            
         DC    AL3(117040),CL4'GOLP'                                            
         DC    AL3(117042),CL4'GOLP'                                            
         DC    AL3(117076),CL4'NFOC'                                            
         DC    AL3(117077),CL4'NFOC'                                            
         DC    AL3(117078),CL4'NFXP'                                            
         DC    AL3(117079),CL4'NFXP'                                            
         DC    AL3(117080),CL4'NFXP'                                            
         DC    AL3(117081),CL4'NFXP'                                            
         DC    AL3(117082),CL4'NFXP'                                            
         DC    AL3(117083),CL4'NFXP'                                            
         DC    AL3(117084),CL4'NFSB'                                            
         DC    AL3(117085),CL4'NFXP'                                            
         DC    AL3(117086),CL4'NFXP'                                            
         DC    AL3(117087),CL4'NFOA'                                            
         DC    AL3(117233),CL4'GOLS'                                            
         DC    AL3(117307),CL4'ETGA'                                            
         DC    AL3(117313),CL4'ETGA'                                            
         DC    AL3(117473),CL4'GOLS'                                            
         DC    AL3(117532),CL4'NFOT'                                            
         DC    AL3(117536),CL4'NFOT'                                            
         DC    AL3(117557),CL4'FGSK'                                            
         DC    AL3(117558),CL4'FGSK'                                            
         DC    AL3(117567),CL4'NFXP'                                            
         DC    AL3(117568),CL4'NFXP'                                            
         DC    AL3(117628),CL4'MSNA'                                            
         DC    AL3(117629),CL4'MSNA'                                            
         DC    AL3(117632),CL4'SPOA'                                            
         DC    AL3(117633),CL4'SPOA'                                            
         DC    AL3(117634),CL4'BKOC'                                            
         DC    AL3(117698),CL4'GOLS'                                            
         DC    AL3(117715),CL4'GOLP'                                            
         DC    AL3(117854),CL4'GOLP'                                            
         DC    AL3(117860),CL4'MSNB'                                            
         DC    AL3(117861),CL4'MSOC'                                            
         DC    AL3(117862),CL4'MSOC'                                            
         DC    AL3(117863),CL4'MSOC'                                            
         DC    AL3(117864),CL4'MSNA'                                            
         DC    AL3(117872),CL4'GOLS'                                            
         DC    AL3(117880),CL4'FGSK'                                            
         DC    AL3(117894),CL4'FGSK'                                            
         DC    AL3(117905),CL4'MSOT'                                            
         DC    AL3(117974),CL4'MSNA'                                            
         DC    AL3(118040),CL4'GOLS'                                            
         DC    AL3(118054),CL4'SOCC'                                            
         DC    AL3(118060),CL4'MSOT'                                            
         DC    AL3(118196),CL4'GOLP'                                            
         DC    AL3(118197),CL4'GOLP'                                            
         DC    AL3(118201),CL4'GOLS'                                            
         DC    AL3(118230),CL4'WNSP'                                            
         DC    AL3(118231),CL4'MSCT'                                            
         DC    AL3(118297),CL4'WNSP'                                            
         DC    AL3(118300),CL4'NHRS'                                            
         DC    AL3(118318),CL4'NFXP'                                            
         DC    AL3(118366),CL4'CBRS'                                            
         DC    AL3(118386),CL4'CBRS'                                            
         DC    AL3(118387),CL4'CBRS'                                            
         DC    AL3(118407),CL4'CBXP'                                            
         DC    AL3(118430),CL4'CBOC'                                            
         DC    AL3(118431),CL4'CBXP'                                            
         DC    AL3(118432),CL4'WNSP'                                            
         DC    AL3(118433),CL4'MSIR'                                            
         DC    AL3(118442),CL4'GOLS'                                            
         DC    AL3(118451),CL4'SPOT'                                            
         DC    AL3(118455),CL4'SPOT'                                            
         DC    AL3(118456),CL4'SPOT'                                            
         DC    AL3(118462),CL4'CBXP'                                            
         DC    AL3(118464),CL4'CBXP'                                            
         DC    AL3(118465),CL4'CBXP'                                            
         DC    AL3(118525),CL4'SPOT'                                            
         DC    AL3(118526),CL4'SPOT'                                            
         DC    AL3(118527),CL4'GOLS'                                            
         DC    AL3(118534),CL4'SPOT'                                            
         DC    AL3(118548),CL4'GOLL'                                            
         DC    AL3(118553),CL4'GOLL'                                            
         DC    AL3(118613),CL4'CBXP'                                            
         DC    AL3(118721),CL4'FGSK'                                            
         DC    AL3(118726),CL4'SPOT'                                            
         DC    AL3(118775),CL4'MSNB'                                            
         DC    AL3(118819),CL4'MSMC'                                            
         DC    AL3(118820),CL4'MSCT'                                            
         DC    AL3(118821),CL4'MSCT'                                            
         DC    AL3(119093),CL4'NFXP'                                            
         DC    AL3(119247),CL4'GOLC'                                            
         DC    AL3(119252),CL4'GOLS'                                            
         DC    AL3(119280),CL4'WNSP'                                            
         DC    AL3(119349),CL4'MSMC'                                            
         DC    AL3(119358),CL4'TEPR'                                            
         DC    AL3(119362),CL4'GOLS'                                            
         DC    AL3(119385),CL4'TEPR'                                            
         DC    AL3(119432),CL4'MSOC'                                            
         DC    AL3(119438),CL4'MSMC'                                            
         DC    AL3(119439),CL4'SPOA'                                            
         DC    AL3(119583),CL4'GOLS'                                            
         DC    AL3(119593),CL4'NHCP'                                            
         DC    AL3(119594),CL4'NHCP'                                            
         DC    AL3(119663),CL4'MSCT'                                            
         DC    AL3(119723),CL4'GOLS'                                            
         DC    AL3(119758),CL4'HSOC'                                            
         DC    AL3(119759),CL4'HSRC'                                            
         DC    AL3(119761),CL4'BKPO'                                            
         DC    AL3(119779),CL4'FGSK'                                            
         DC    AL3(119862),CL4'GOLP'                                            
         DC    AL3(119863),CL4'GOLP'                                            
         DC    AL3(119887),CL4'GOLS'                                            
         DC    AL3(119890),CL4'BKPO'                                            
         DC    AL3(119914),CL4'NHCP'                                            
         DC    AL3(120079),CL4'HSOC'                                            
         DC    AL3(120080),CL4'HSRC'                                            
         DC    AL3(120081),CL4'BKPO'                                            
         DC    AL3(120144),CL4'MSCT'                                            
         DC    AL3(120145),CL4'NHCP'                                            
         DC    AL3(120146),CL4'NHOC'                                            
         DC    AL3(120155),CL4'BKPO'                                            
         DC    AL3(120266),CL4'BKPO'                                            
         DC    AL3(120278),CL4'GOLS'                                            
         DC    AL3(120294),CL4'MSMC'                                            
         DC    AL3(120314),CL4'BKXP'                                            
         DC    AL3(120315),CL4'BKPO'                                            
         DC    AL3(120357),CL4'BKXP'                                            
         DC    AL3(120388),CL4'MSNA'                                            
         DC    AL3(120392),CL4'GOLO'                                            
         DC    AL3(120425),CL4'BKPO'                                            
         DC    AL3(120494),CL4'HSOC'                                            
         DC    AL3(120495),CL4'HSRC'                                            
         DC    AL3(120540),CL4'GOLS'                                            
         DC    AL3(120542),CL4'NHCP'                                            
         DC    AL3(120544),CL4'MSCT'                                            
         DC    AL3(120564),CL4'SOCC'                                            
         DC    AL3(120602),CL4'GOLS'                                            
         DC    AL3(120723),CL4'GOLP'                                            
         DC    AL3(120822),CL4'GOOT'                                            
         DC    AL3(120956),CL4'GOLS'                                            
         DC    AL3(120958),CL4'MSIR'                                            
         DC    AL3(121131),CL4'BKPO'                                            
         DC    AL3(121132),CL4'BKPO'                                            
         DC    AL3(121133),CL4'BKPO'                                            
         DC    AL3(121140),CL4'BBOC'                                            
         DC    AL3(121152),CL4'MSOC'                                            
         DC    AL3(121153),CL4'MSNA'                                            
         DC    AL3(121156),CL4'MSOC'                                            
         DC    AL3(121211),CL4'TEPR'                                            
         DC    AL3(121228),CL4'BBXP'                                            
         DC    AL3(121285),CL4'MSNB'                                            
         DC    AL3(121286),CL4'MSOC'                                            
         DC    AL3(121287),CL4'MSNA'                                            
         DC    AL3(121307),CL4'GOLP'                                            
         DC    AL3(121308),CL4'GOLC'                                            
         DC    AL3(121368),CL4'GOLC'                                            
         DC    AL3(121369),CL4'GOLC'                                            
         DC    AL3(121377),CL4'GOLS'                                            
         DC    AL3(121423),CL4'GOLL'                                            
         DC    AL3(121424),CL4'GOLL'                                            
         DC    AL3(121429),CL4'SPOT'                                            
         DC    AL3(121610),CL4'GOLS'                                            
         DC    AL3(121616),CL4'MSNA'                                            
         DC    AL3(121661),CL4'GOLL'                                            
         DC    AL3(121663),CL4'GOLL'                                            
         DC    AL3(121667),CL4'TRFD'                                            
         DC    AL3(121668),CL4'TRFD'                                            
         DC    AL3(121690),CL4'MSNB'                                            
         DC    AL3(121844),CL4'GOLS'                                            
         DC    AL3(121849),CL4'MSIR'                                            
         DC    AL3(121851),CL4'TRFD'                                            
         DC    AL3(121852),CL4'NFOT'                                            
         DC    AL3(121853),CL4'TRFD'                                            
         DC    AL3(121956),CL4'GOLS'                                            
         DC    AL3(121979),CL4'ETGA'                                            
         DC    AL3(121980),CL4'ETGA'                                            
         DC    AL3(121981),CL4'NFOT'                                            
         DC    AL3(121982),CL4'ETGA'                                            
         DC    AL3(121983),CL4'ETGA'                                            
         DC    AL3(121995),CL4'MSOT'                                            
         DC    AL3(121997),CL4'MSNA'                                            
         DC    AL3(122007),CL4'MSOC'                                            
         DC    AL3(122009),CL4'BBOT'                                            
         DC    AL3(122090),CL4'SPOT'                                            
         DC    AL3(122092),CL4'FCXP'                                            
         DC    AL3(122095),CL4'SPOT'                                            
         DC    AL3(122223),CL4'GOLS'                                            
         DC    AL3(122228),CL4'MSIR'                                            
         DC    AL3(122239),CL4'NFXP'                                            
         DC    AL3(122240),CL4'NFXP'                                            
         DC    AL3(122403),CL4'SPOA'                                            
         DC    AL3(122404),CL4'SPOA'                                            
         DC    AL3(122405),CL4'SPOA'                                            
         DC    AL3(122450),CL4'FCRS'                                            
         DC    AL3(122472),CL4'WRST'                                            
         DC    AL3(122500),CL4'GOLS'                                            
         DC    AL3(122609),CL4'GOLL'                                            
         DC    AL3(122610),CL4'GOLL'                                            
         DC    AL3(122673),CL4'NFOT'                                            
         DC    AL3(122859),CL4'WRST'                                            
         DC    AL3(122912),CL4'GOLS'                                            
         DC    AL3(123313),CL4'MSNA'                                            
         DC    AL3(123314),CL4'MSOC'                                            
         DC    AL3(123392),CL4'WRST'                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* SYNDICATION TABLES                                                            
SYNSUBP  DS    0H                                                               
         DC    AL3(2658),CL4'WRST'                                              
         DC    AL3(53622),CL4'NS'                                               
         DC    AL3(62425),CL4'WRST'                                             
         DC    AL3(64341),CL4'NFRS'                                             
         DC    AL3(74978),CL4'SPOT'                                             
         DC    AL3(77161),CL4'BBOT'                                             
         DC    AL3(80734),CL4'SPOC'                                             
         DC    AL3(87694),CL4'SPOT'                                             
         DC    AL3(90634),CL4'SPOT'                                             
         DC    AL3(92891),CL4'BBPO'                                             
         DC    AL3(92892),CL4'BBPO'                                             
         DC    AL3(92893),CL4'BBPO'                                             
         DC    AL3(92894),CL4'BBPO'                                             
         DC    AL3(92895),CL4'BBPO'                                             
         DC    AL3(92896),CL4'BBPO'                                             
         DC    AL3(92901),CL4'SPOA'                                             
         DC    AL3(93073),CL4'BBPO'                                             
         DC    AL3(97282),CL4'WNSP'                                             
         DC    AL3(98258),CL4'WNSP'                                             
         DC    AL3(101804),CL4'GOLC'                                            
         DC    AL3(102350),CL4'SPOT'                                            
         DC    AL3(102616),CL4'NFOC'                                            
         DC    AL3(103084),CL4'BBPO'                                            
         DC    AL3(103469),CL4'NFRS'                                            
         DC    AL3(106110),CL4'GOLC'                                            
         DC    AL3(106112),CL4'FCOC'                                            
         DC    AL3(106128),CL4'WRST'                                            
         DC    AL3(107191),CL4'SPOA'                                            
         DC    AL3(108004),CL4'SPOA'                                            
         DC    AL3(108384),CL4'GOLC'                                            
         DC    AL3(110010),CL4'SPOA'                                            
         DC    AL3(110700),CL4'BBOC'                                            
         DC    AL3(111683),CL4'TENA'                                            
         DC    AL3(111761),CL4'NFPS'                                            
         DC    AL3(111938),CL4'SPOA'                                            
         DC    AL3(112250),CL4'SPOT'                                            
         DC    AL3(112945),CL4'SPOT'                                            
         DC    AL3(113391),CL4'FCOC'                                            
         DC    AL3(113849),CL4'WRST'                                            
         DC    AL3(113854),CL4'SPOT'                                            
         DC    AL3(116869),CL4'FCOC'                                            
         DC    AL3(117003),CL4'FGSK'                                            
         DC    AL3(117985),CL4'SPOA'                                            
         DC    AL3(118081),CL4'MSNA'                                            
         DC    AL3(118171),CL4'WNSP'                                            
         DC    AL3(118518),CL4'WNSP'                                            
         DC    AL3(118776),CL4'SPOA'                                            
         DC    AL3(120748),CL4'FGSK'                                            
         DC    AL3(120858),CL4'BKOA'                                            
         DC    AL3(121514),CL4'SPOT'                                            
         DC    AL3(121609),CL4'NFPS'                                            
         DC    AL3(122690),CL4'FCOC'                                            
         DC    X'FF'                                                            
         EJECT                                                                  
NHSITAB  DS    0H                                                               
         DC    AL2(390),AL1(00,00) ALBUQUERQUE                                  
         DC    AL2(235),AL1(03,02) AUSTIN,TX                                    
         DC    AL2(202),AL1(00,00) CHICAGO                                      
         DC    AL2(200),AL1(00,00) CORPUS CHRISTI                               
         DC    AL2(223),AL1(00,00) DALLAS-FT WORTH                              
         DC    AL2(351),AL1(03,11) DENVER                                       
         DC    AL2(365),AL1(00,00) EL PASO                                      
         DC    AL2(466),AL1(00,00) FRESNO(VISALIA)                              
         DC    AL2(236),AL1(00,00) HARLINGEN-WESLACO                            
         DC    AL2(218),AL1(00,00) HOUSTON                                      
         DC    AL2(403),AL1(00,00) LOS ANGELES                                  
         DC    AL2(128),AL1(00,00) MIAMI-FT LAUDERDALE                          
         DC    AL2(101),AL1(00,00) NEW YORK                                     
         DC    AL2(353),AL1(00,00) PHOENIX                                      
         DC    AL2(462),AL1(00,00) SACRAMENTO-STOCKTON                          
         DC    AL2(241),AL1(00,00) SAN ANTONIO                                  
         DC    AL2(425),AL1(00,00) SAN DIEGO                                    
         DC    AL2(407),AL1(00,00) SAN FRANCISCO-OAKLAND                        
         DC    AL2(389),AL1(00,00) TUCSON                                       
         DC    X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
* TABLES OF NETWORK LONG NAMES BY MEDIA                                         
***********************************************************************         
                                                                                
LNETTAB  DS    0X                                                               
         DC    C'CN',C'C'                       CABLE                           
         DC    AL3(0),AL1(NECABNAM),AL1(0),AL1(L'NECBNNML)                      
         DC    AL1(NECBNNML-NECBNAMD),AL1(NECBNNAM-NECBNAMD)                    
         DC    AL4(ECABNET)                                                     
                                                                                
         DC    C'CN',C'C'  (IF CABLE NOT FOUND) HISPANIC CABLE                  
         DC    AL3(0),AL1(NEHCBNAM),AL1(0),AL1(L'NEHCNNML)                      
         DC    AL1(NEHCNNML-NEHCNAMD),AL1(NEHCNNAM-NEHCNAMD)                    
         DC    AL4(ENHCNET)                                                     
                                                                                
         DC    C'CN',C'Q'                       CABLE QH DATA                   
         DC    AL3(0),AL1(NECABNAM),AL1(0),AL1(L'NECBNNML)                      
         DC    AL1(NECBNNML-NECBNAMD),AL1(NECBNNAM-NECBNAMD)                    
         DC    AL4(ECABNET)                                                     
                                                                                
         DC    C'CN',C'Q'  (IF CABLE NOT FOUND) HISPANIC QH CABLE               
         DC    AL3(0),AL1(NEHCBNAM),AL1(0),AL1(L'NEHCNNML)                      
         DC    AL1(NEHCNNML-NEHCNAMD),AL1(NEHCNNAM-NEHCNAMD)                    
         DC    AL4(ENHCNET)                                                     
                                                                                
         DC    C'NN',C'T'                       BROADCAST                       
         DC    AL3(0),AL1(NEBROD),AL1(0),AL1(L'NEBRDNET)                        
         DC    AL1(NEBRDNET-NEBRDNMD),AL1(NEBRDNAM-NEBRDNMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'NN',C'T'   (IF BROADC NOT FND) BROADCAST AGGREGATES            
         DC    AL3(0),AL1(NEBAGG),AL1(0),AL1(L'NEBAGNET)                        
         DC    AL1(NEBAGNET-NEBAGNMD),AL1(NEBAGNAM-NEBAGNMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'NN',C'D'                       DAILIES                         
         DC    AL3(0),AL1(NEBROD),AL1(0),AL1(L'NEBRDNET)                        
         DC    AL1(NEBRDNET-NEBRDNMD),AL1(NEBRDNAM-NEBRDNMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'NN',C'D'  (IF DAILIES NOT FND) DAILIES AGGREGATES              
         DC    AL3(0),AL1(NEBAGG),AL1(0),AL1(L'NEBAGNET)                        
         DC    AL1(NEBAGNET-NEBAGNMD),AL1(NEBAGNAM-NEBAGNMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'NN',C'H'              HISPANIC BROADCAST/TV AUDIENCE           
         DC    AL3(0),AL1(NEHBRO),AL1(0),AL1(L'NEHBRNET)                        
         DC    AL1(NEHBRNET-NEHBRNMD),AL1(NEHBRNAM-NEHBRNMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'NN',C'H'   (IF HISP BROD NOT FOUND) HISP GENERAL MKTS          
         DC    AL3(0),AL1(NEHGES),AL1(0),AL1(L'NEHGENET)                        
         DC    AL1(NEHGENET-NEHGENMD),AL1(NEHGENAM-NEHGENMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'NN',C'H'   (IF HISP NOT FOUND) HISP TVA AGGREGATES             
         DC    AL3(0),AL1(NEHTAG),AL1(0),AL1(L'NEHTAGNT)                        
         DC    AL1(NEHTAGNT-NEHTAGD),AL1(NEHTAGNM-NEHTAGD)                      
         DC    AL4(0)                                                           
                                                                                
         DC    C'NN',C'H'   (IF HISP NOT FOUND) HISP GEN MKT AGGREGATES         
         DC    AL3(0),AL1(NEHGAG),AL1(0),AL1(L'NEHGAGNT)                        
         DC    AL1(NEHGAGNT-NEHGAGD),AL1(NEHGAGNM-NEHGAGD)                      
         DC    AL4(0)                                                           
                                                                                
         DC    C'NN',C'N'                       NAD                             
         DC    AL3(0),AL1(NEBROD),AL1(0),AL1(L'NEBRDNET)                        
         DC    AL1(NEBRDNET-NEBRDNMD),AL1(NEBRDNAM-NEBRDNMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'NN',C'S'                       SYNDICATION                     
         DC    AL3(0),AL1(NESYND),AL1(0),AL1(L'NESYNNET)                        
         DC    AL1(NESYNNET-NESYNNMD),AL1(NESYNNAM-NESYNNMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'NN',C'M'                       NAD-SYNDICATION                 
         DC    AL3(0),AL1(NESYND),AL1(0),AL1(L'NESYNNET)                        
         DC    AL1(NESYNNET-NESYNNMD),AL1(NESYNNAM-NESYNNMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'WN',C'H'       WEEKLY HISPANIC BROADCAST/TV AUDIENCE           
         DC    AL3(0),AL1(NEHBRO),AL1(0),AL1(L'NEHBRNET)                        
         DC    AL1(NEHBRNET-NEHBRNMD),AL1(NEHBRNAM-NEHBRNMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'WN',C'H'   (IF HISP BROD NOT FOUND) WKLY HISP GEN MKTS         
         DC    AL3(0),AL1(NEHGES),AL1(0),AL1(L'NEHGENET)                        
         DC    AL1(NEHGENET-NEHGENMD),AL1(NEHGENAM-NEHGENMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'WN',C'H'   (IF HISP NOT FOUND) WKLY HISP TVA AGGRGATES         
         DC    AL3(0),AL1(NEHTAG),AL1(0),AL1(L'NEHTAGNT)                        
         DC    AL1(NEHTAGNT-NEHTAGD),AL1(NEHTAGNM-NEHTAGD)                      
         DC    AL4(0)                                                           
                                                                                
         DC    C'WN',C'H'   (IF HISP NOT FOUND) WKLY HISP GEN MKT AGRTS         
         DC    AL3(0),AL1(NEHGAG),AL1(0),AL1(L'NEHGAGNT)                        
         DC    AL1(NEHGAGNT-NEHGAGD),AL1(NEHGAGNM-NEHGAGD)                      
         DC    AL4(0)                                                           
                                                                                
         DC    C'WN',C'N'                       NAD WEEKLY                      
         DC    AL3(0),AL1(NEBROD),AL1(0),AL1(L'NEBRDNET)                        
         DC    AL1(NEBRDNET-NEBRDNMD),AL1(NEBRDNAM-NEBRDNMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'WN',C'M'                       NAD-SYNDICATION WEEKLY          
         DC    AL3(0),AL1(NESYND),AL1(0),AL1(L'NESYNNET)                        
         DC    AL1(NESYNNET-NESYNNMD),AL1(NESYNNAM-NESYNNMD)                    
         DC    AL4(0)                                                           
                                                                                
         DC    C'RN',C'Q'                       RENTRAK                         
         DC    AL3(0),AL1(RECABNAM),AL1(0),AL1(L'RECBNNUM)                      
         DC    AL1(RECBNNUM-RECBNAMD),AL1(RECBNNAM-RECBNAMD)                    
         DC    AL4(ECABREN)                                                     
                                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* TABLES OF PARAMETER KEYWORDS                                                  
***********************************************************************         
                                                                                
KEYDTAB  DS    0H                                                               
         DC    CL8'NLIV    ',AL4(NLIV),AL1(REL)                                 
         DC    CL8'PROG    ',AL4(PROG),AL1(0)                                   
         DC    CL8'TIME    ',AL4(TIME),AL1(0)                                   
         DC    CL8'DAY     ',AL4(DAY),AL1(0)                                    
         DC    CL8'CPROG   ',AL4(CPROG),AL1(0)                                  
         DC    CL8'TYPE    ',AL4(TYPE),AL1(0)                                   
         DC    CL8'PTYPE   ',AL4(PTYPE),AL1(0)                                  
         DC    CL8'PTYP4   ',AL4(PTYP4),AL1(0)                                  
         DC    CL8'PSOUR   ',AL4(PSOUR),AL1(0)                                  
         DC    CL8'TRAK    ',AL4(TRAK),AL1(0)                                   
         DC    CL8'TPNO    ',AL4(TPNO),AL1(0)                                   
         DC    CL8'PVNO    ',AL4(PVNO),AL1(0)                                   
         DC    CL8'AFFL    ',AL4(AFFL),AL1(0)                                   
         DC    CL8'NTIL    ',AL4(NTIL),AL1(0)                                   
         DC    CL8'NTI     ',AL4(NTI),AL1(0)                                    
         DC    CL8'NET     ',AL4(NET),AL1(0)                                    
         DC    CL8'PURE    ',AL4(PURE),AL1(0)                                   
         DC    CL8'NAD     ',AL4(NTI),AL1(0)                                    
         DC    CL8'COVER   ',AL4(COVER),AL1(0)                                  
         DC    CL8'SCOUN   ',AL4(SCOUNT),AL1(0)                                 
         DC    CL8'MAR     ',AL4(MAR),AL1(0)                                    
         DC    CL8'ANMKT   ',AL4(ALFNUMMK),AL1(0)                               
         DC    CL8'NAMKT   ',AL4(ALFNUMMK),AL1(0)                               
         DC    CL8'NAXMKT  ',AL4(ALFNUMMK),AL1(0)                               
         DC    CL8'MNA     ',AL4(MNAME),AL1(0)                                  
         DC    CL8'TIMX    ',AL4(TIMX),AL1(0)                                   
         DC    CL8'ADYT    ',AL4(ADYTM),AL1(0)                                  
         DC    CL8'WEEK    ',AL4(WEEK),AL1(0)                                   
         DC    CL8'TOT2DUR ',AL4(TOT2DUR),AL1(0)                                
         DC    CL8'TOTD    ',AL4(TOTDUR),AL1(0)                                 
         DC    CL8'CORR    ',AL4(CORRECT),AL1(0)                                
         DC    CL8'DPT     ',AL4(DAYPART),AL1(0)                                
         DC    CL8'STATE   ',AL4(STATE),AL1(REL)                                
         DC    CL8'STANM   ',AL4(STATE),AL1(REL)                                
         DC    CL8'STAT    ',AL4(STATION),AL1(REL)                              
         DC    CL8'GAA     ',AL4(GAA),AL1(0)                                    
         DC    CL8'PREM    ',AL4(PREM),AL1(REL)                                 
         DC    CL8'NDMON   ',AL4(NDMON),AL1(0)                                  
         DC    CL8'NDAYS   ',AL4(NDAYS),AL1(0)                                  
         DC    CL8'EPIS    ',AL4(EPIS),AL1(REL)                                 
         DC    CL8'FEED    ',AL4(FEED),AL1(REL)                                 
         DC    CL8'LIVE    ',AL4(LIVE),AL1(REL)                                 
         DC    CL8'MP$     ',AL4(MPCOST),AL1(REL)                               
         DC    CL8'COMM    ',AL4(COMMST),AL1(REL)                               
         DC    CL8'CTYN    ',AL4(CTYINFO),AL1(REL)                              
         DC    CL8'CTY#    ',AL4(CTYINFO),AL1(REL)                              
         DC    CL8'DMA#    ',AL4(DMAINFO),AL1(REL)    DMA NUMBER (3)            
         DC    CL8'DMAO    ',AL4(DMAINFO),AL1(REL)    DMA OF ORIGIN (3)         
         DC    CL8'DMAN    ',AL4(DMAINFO),AL1(REL)    DMA NAME (26)             
         DC    CL8'BKOUT   ',AL4(BKOUT),AL1(REL)      BREAKOUT PGM?             
         DC    CL8'PNEW    ',AL4(PGRECD),AL1(REL)     PGM RECD NEW              
         DC    CL8'PTIER   ',AL4(PGRECD),AL1(REL)     PGM RECD TIER             
         DC    CL8'PRAT    ',AL4(PGRECD),AL1(REL)     PGM RECD RATNG CD         
         DC    CL8'UYEAR   ',AL4(UYEAR),AL1(REL)      UNIVERSE YEAR             
         DC    CL8'LNET    ',AL4(LNET),AL1(REL)       LONG NETWK NAME           
         DC    CL8'LDDAT   ',AL4(LDDAT),AL1(REL)      LOAD DATE                 
         DC    CL8'LWEEK   ',AL4(LWEEK),AL1(REL)      5-WEEK FIELD              
         DC    CL8'COMS    ',AL4(COMSEC),AL1(REL)     COMM AVG SECONDS          
         DC    CL8'COMT    ',AL4(COMTEL),AL1(REL)     COMM AVG TELCASTS         
         DC    CL8'MBINF   ',AL4(MBINF),AL1(REL)      COMM DURATION             
         DC    CL8'PRMIN   ',AL4(PROGMIN),AL1(REL)    PROGRAM MINUTE            
         DC    CL8'DURATION',AL4(DURATION),AL1(REL)   DURATION IN MINS          
         DC    CL8'MCOMSEC ',AL4(MCOMSEC),AL1(REL)    MINUTE COMM SECS          
         DC    CL8'PODINFO ',AL4(PODINFO),AL1(REL)    POD INFO                  
         DC    CL8'GAPIND  ',AL4(GAPIND),AL1(REL)     GAPPED INDICATOR          
         DC    CL8'MOP     ',AL4(MOP),AL1(REL)        MOP                       
         DC    CL8'TRKTELNO',AL4(TRKTELNO),AL1(REL)   TRK+TCAST NUMBERS         
         DC    CL8'CORAMRLD',AL4(CORAMRLD),AL1(0)     CORR INFO FOR MXM         
         DC    CL8'CNTDAYS ',AL4(CNTDAYS),AL1(REL)    COUNT NO OF DAYS          
         DC    CL8'OSUSTAIN',AL4(OSUSTAIN),AL1(REL)   ORD SUSTAINER IND         
         DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
         TITLE '-   DEMO RECORD DATA EXTRACTOR (DSECTS)'                        
*              DSECTS                                                           
KEYDTABD DSECT                     DSECT TO COVER KEYWORD TABLE                 
KEYWORD  DS    CL8                 REQUESTED PARAMETERE KEYWORD                 
KEYROUT  DS    AL4                 ROUTINE TO PROCESS KEYWORD                   
KEYFLAG  DS    X                   FLAGS                                        
REL      EQU   X'80'               ROUTINE IS RELOCATED                         
KEYDTABL EQU   *-KEYDTABD                                                       
                                                                                
FINED    DSECT                                                                  
DUB      DS    D                                                                
DUB2     DS    D                                                                
ALET     DS    A                                                                
RELO     DS    A                                                                
DMCB     DS    6F                                                               
MYBASE1  DS    A                   A(1ST 4096 BYTES OF THIS PROGRAM)            
MYBASE2  DS    A                   A(2ND 4096 BYTES OF THIS PROGRAM)            
MINUTE   DS    F                   # OF MINUTES (BETWEEN 0 & 59)                
HOUR     DS    F                   5AM RADIO/6AM TV BASED HOUR NUMBER           
ANETWEEK DS    A                   A(NETWEEK) FROM CALLOV                       
AGOSUB   DS    A                   A(SUBROUTINE POOL INTERFACE)                 
WORK     DS    CL32                                                             
WORKL    DS    CL255                                                            
DATADISP DS    H                                                                
ELCODE   DS    CL1                                                              
MTIME    DS    XL2                 MILITARY TIME                                
MYDBFILE DS    CL3                 FUDGABILITY FOR NET-SPOT INTERFACE           
SPNTFLG  DS    X                   SPOT - NETW LK UP                            
GOSUBN   DS    XL1                 SUBROUTINE NUMBER                            
CBLNAD   DS    HL1                 CABLE NAD FLAG (1=CABLE NAD)                 
RECBOOK  DS    XL2                 RECORD BOOK                                  
FINEX    EQU   *                                                                
                                                                                
LNETTABD DSECT                                                                  
LNETMED  DS    C                   MEDIA ON THE DEMO RECORD                     
LNETSRC  DS    C                   SOURCE ON THE DEMO RECORD                    
LNETC5   DS    C                   5TH CHARACTER OF THE STATION                 
LNETATAB DS    AL4                 A(TABLE OF NETWORK NAMES)                    
*                                  OR                                           
*                                  IF TABLE LIVES IN DEMTABS,                   
*                                  AL3(0)+AL1(PARAMETER TO DEMTABS)             
LNETLEN  DS    AL1                 LENGTH OF TABLE ENTRY                        
*                                                                               
LNETNLEN DS    AL1                 LENGTH OF NETWORK CODE                       
LNETNETD DS    AL1                 DISPLACEMENT TO NETWORK CODE                 
LNETNAMD DS    AL1                 DISPLACEMENT TO NETWORK NAME                 
LNETAROU DS    AL4                 ROUTINE TO COMPARE THE NETWK                 
LNETTABL EQU   *-LNETTABD                                                       
                                                                                
NETRANGE DSECT                                                                  
NETMIN   DS    H                                                                
NETMAX   DS    H                                                                
NETCALL  DS    CL4                                                              
NETLEN   EQU   *-NETMIN                                                         
                                                                                
DBLOCKD  DSECT                                                                  
         EJECT                                                                  
       ++INCLUDE DEMPAFILE                                                      
       ++INCLUDE DEDBLOCK                                                       
DBLOCKL  EQU   *-DBLOCK                                                         
*        PRINT OFF                                                              
* DEDEMFILE                                                                     
       ++INCLUDE DEDEMFILE                                                      
* DEDBEXTRAD                                                                    
       ++INCLUDE DEDBEXTRAD                                                     
* REGENINVA                                                                     
       ++INCLUDE REGENINVA                                                      
* DEDEMTABD                                                                     
       ++INCLUDE DEDEMTABD                                                      
* FASYSFAC                                                                      
       ++INCLUDE FASYSFAC                                                       
* FASSB                                                                         
       ++INCLUDE FASSB                                                          
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* DDMASTD                                                                       
       ++INCLUDE DDMASTD                                                        
* SPGENPROG                                                                     
       ++INCLUDE SPGENPROG                                                      
* DEDEMLNETD                                                                    
       ++INCLUDE DEDEMLNETD                                                     
* DDMONYREQU                                                                    
       ++INCLUDE DDMONYREQU                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'104DEDEFINE  01/11/21'                                      
         END                                                                    

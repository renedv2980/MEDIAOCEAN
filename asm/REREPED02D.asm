*          DATA SET REREPED02D AT LEVEL 030 AS OF 06/19/97                      
*PHASE REED02A,*                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE BINSRCH                                                                
*INCLUDE DMPRTQB                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREPED02 (REED02) --- KATZ EDI TRANSFER '                      
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPED02A -- KATZ EDI TRANSFER:  ACCEPT A TAPE FILE,     *            
*                      CONVERT DATA TO DARE FORMAT RECORDS, PASS   *            
*                      INTO DARE VIA EDICT                         *            
*                                                                  *            
*        SPECIAL SWEEP FOR CONTRACT NUMBERS VERSION!!              *            
*                                                                  *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* JAN18/96 (BU ) --- ORIGINAL ENTRY                                *            
*                                                                  *            
* MAY14/96 (BU ) --- MODIFY FOR KATZ TV USE                        *            
*                                                                  *            
* MAY29/96 (BU ) --- CONTROL FILE VERSION, FOR COMPANY DETER-      *            
*                    MINATION                                      *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REED02   CSECT                                                                  
         NMOD1 0,**REED**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
*                                                                               
         GOTO1 =V(STXITER),DMCB,DUMPLIST                                        
*                                                                               
         XC    PQFLAG,PQFLAG                                                    
*                                                                               
         L     R8,REMOTEC                                                       
         USING REMOTED,R8                                                       
         MVC   SAVID,REMOTDST                                                   
         DROP  R8                                                               
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   MAINEXIT                                                         
         GOTO1 INITIAL,DMCB,(RC)                                                
*                                                                               
         XC    BUYLINE,BUYLINE                                                  
         XC    TOTRECS,TOTRECS                                                  
         XC    TOTSPOT,TOTSPOT                                                  
         XC    TOTDLLRS,TOTDLLRS                                                
         L     R4,ARECAREA         SET A(TAPE RECD DELIVERY AREA)               
         USING DKHEADR1,R4                                                      
MAIN     EQU   *                                                                
         GET   INTAPE,(R4)         READ TAPE RECORD INTO RDA                    
         CLI   DKHDTYP,DKH2TYPQ    HEADER TWO RECORD                            
         BE    MAIN0200                                                         
         CLI   DKHDTYP,DKH3TYPQ    HEADER THREE RECORD                          
         BE    MAIN0360                                                         
         CLI   DKHDTYP,DKDETYPQ    DETAIL OR DETAIL COMMENT RECORD              
         BNE   MAIN0020                                                         
         CLC   DKHDNAME(4),=C'9999'    DETAIL COMMENT RECORD                    
         BNE   MAIN0420            DETAIL RECORD                                
         BE    MAIN0800            DETAIL COMMENT RECORD                        
MAIN0020 CLI   DKHDTYP,DKTTYPQ     TLRTLR RECORD                                
         BE    MAIN0860                                                         
         CLI   DKHDTYP,DKHDTYPQ    HEADER ONE RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  EOF --> MAIN0900                             
MAIN0040 DS    0H                                                               
*        CLC   =C'KVEO',DKHDSTAT   SINGLE-STATION CHECK                         
*        BE    MAIN004X            FOUND: PROCESS                               
         CLC   =C'WCVB',DKHDSTAT   SINGLE-STATION CHECK                         
         BNE   MAIN0204            SKIP ORDER:  READ FOR NEXT HEADER            
MAIN004X DS    0H                                                               
*                                                                               
*   TEST                                                                        
*        MVC   P+1(04),DKHDSTAT                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLI   QOPTION2,C' '       RECORD # CUTOFF?                             
         BE    MAIN0050            NO                                           
         ZIC   RF,QOPTION2         YES - IT'S A CUTOFF COUNTER                  
         SLL   RF,28               DROP ZONE BITS                               
         SRL   RF,28               REALIGN COUNTER                              
         C     RF,RECCTR           CUTOFF VS REC COUNT                          
         BNH   MAIN0900            COUNTER  DONE: EXIT                          
MAIN0050 EQU   *                                                                
         BAS   RE,BMPREC                                                        
*                                                                               
*   TEST                                                                        
         MVC   P+1(10),=C'REF INPUT:'                                           
         MVC   P+12(1),DKHDTYP                                                  
         MVC   P+14(25),DKHDREF                                                 
         GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
*        CLC   DKHDREF+2(8),=C'10082776'                                        
*        BE    TEST0020                                                         
*        CLC   DKHDREF+2(8),=C'00827767'                                        
*        BNE   TEST0040                                                         
*C'00658091'                                                                    
*C'00658194'                                                                    
*C'00658235'                                                                    
*C'00658147'                                                                    
*C'00658212'                                                                    
*C'00658215'                                                                    
*C'00658104'                                                                    
TEST0020 EQU   *                                                                
         MVC   P+1(10),=C'REF FOUND:'                                           
         MVC   P+12(8),DKHDREF+2                                                
         GOTO1 REPORT                                                           
TEST0040 EQU   *                                                                
         LA    R5,AGYDS1                                                        
         USING PAGYDS1D,R5                                                      
         MVC   PAD1TID,=C'AGYDS1'                                               
         MVC   PAD1ORDR,DKHDREF+2  REFERENCE NUMBER                             
         OC    PAD1ORDR,ZEROES                                                  
         MVC   PAD1AGAD(5),=C'$EDI$'                                            
         MVC   PAD1AGAD+5(L'DKHDREF),DKHDREF                                    
         MVC   PAD1AGAD+30(L'DKHDMKT),DKHDMKT                                   
         MVC   PAD1AGAD+32(L'DKHDPOB),DKHDPOB                                   
*                                                                               
         DROP  R5                  AGENCY DESCRIPTION LINE ONE NOT DONE         
*                                                                               
         BAS   RE,BMPREC                                                        
         LA    R5,AGYDS2                                                        
         USING PAGYDS2D,R5                                                      
         MVC   PAD2TID,=C'AGYDS2'                                               
         MVC   PAD2ORDR,DKHDREF+2                                               
         OC    PAD2ORDR,ZEROES                                                  
         MVC   PAD2TDEM(L'DKHDDEMO),DKHDDEMO                                    
*                                                                               
         MVC   PAD2CLTN(L'DKHDADNM),DKHDADNM                                    
         DROP  R5                  AGENCY DESCRIPTION LINE 2 NOT DONE           
*                                                                               
         BAS   RE,BMPREC                                                        
         LA    R5,AGYDS3                                                        
         USING PAGYDS3D,R5                                                      
         MVC   PAD3TID,=C'AGYDS3'                                               
         MVC   PAD3ORDR,DKHDREF+2                                               
         OC    PAD3ORDR,ZEROES                                                  
         MVC   PAD3PRDN(L'DKHDPRNM),DKHDPRNM                                    
         MVC   PAD3PR2N+24(L'DKHDESNO),DKHDESNO                                 
         DROP  R5                  DONE WITH AGENCY DESCRIPTION LINE 3          
*                                                                               
         BAS   RE,BMPREC                                                        
         XC    AGYDS4,AGYDS4                                                    
         LA    R5,AGYDS4                                                        
         USING PAGYDS4D,R5                                                      
         MVC   PAD4TID,=C'AGYDS4'                                               
         MVC   PAD4ORDR,DKHDREF+2                                               
         OC    PAD4ORDR,ZEROES                                                  
         XC    PAD4BUYR,PAD4BUYR                                                
         MVC   PAD4BYRN,DKHDBUYR                                                
*                                                                               
         MVC   PAD4BYRT,DKHDBUFN                                                
         CLI   PAD4BYRT+3,C'-'                                                  
         BE    *+12                                                             
         CLI   PAD4BYRT+3,C' '                                                  
         BNE   MAIN0060                                                         
         MVC   PAD4BYRT+3(3),DKHDBUFN+4                                         
         MVC   PAD4BYRT+6(4),DKHDBUFN+8                                         
         B     *+10                                                             
MAIN0060 MVC   PAD4BYRX(4),DKHDBUFN+10                                          
         DROP  R5                  DONE WITH AGENCY DESCRIPTION LINE 4          
*                                                                               
         BAS   RE,BMPREC                                                        
         LA    R5,AGYHDR                                                        
         USING PAGYHDRD,R5         AGENCY HEADER LINE                           
         MVC   PAHDTID,=C'AGYHDR'                                               
         MVC   PAHDORDR,DKHDREF+2                                               
         OC    PAHDORDR,ZEROES                                                  
*                                                                               
         MVI   PAHDVERS,C'0'                                                    
         MVC   PAHDRTRN,DKHDMKNM   MARKET NAME                                  
         MVC   PAHDFRID+5(5),DKHDOFNM                                           
         MVC   PAHDROUT+3(2),DKHDOFF                                            
*                                                                               
         MVC   AGYEDI+35(L'DKHDMKNM),DKHDMKNM  MARKET NAME                      
*                                                                               
         MVC   PAHDDATE,DKHDDATE                                                
         MVC   PAHDTIME,DKHDTIME                                                
         MVC   PAHDQSTA(L'DKHDSTAT),DKHDSTAT                                    
         CLI   PAHDQSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   PAHDQSTA+4,C'T'     FORCE TV IF SPACE                            
         GOTO1 GETCOMP,DMCB,(R4)                                                
         MVC   PAHDQEST,DKHDESNO                                                
         CLC   DKHDESNO+6(2),SPACES                                             
         BNH   *+6                                                              
         DC    H'0'                IF THEIR EST IS > OURS -> UHOH               
*                                                                               
         XC    HIAREC,HIAREC                                                    
         MVC   PAHDESTS,DKHDST1                                                 
         MVC   PAHDESTN,DKHDEND3                                                
         CLC   DKHDEND3,SPACES     PUT LAST EXISTING END DATE                   
         BH    MAIN0080            DO A HIATUS                                  
         MVC   PAHDESTN,DKHDEND2                                                
         CLC   DKHDEND2,SPACES                                                  
         BH    MAIN0080            DO A HIATUS                                  
         XC    HIAREC,HIAREC                                                    
         MVC   PAHDESTN,DKHDEND1                                                
         B     MAIN0160            NO NEED TO DO ANY HIATUSES                   
*                                                                               
MAIN0080 DS    0H                  DO ALL AGENCY HIATUS WEEK LINES              
         LA    R7,HIAREC                                                        
         USING PAGYHIAD,R7                                                      
         MVC   PAHITID,=C'AGYHIA'                                               
         MVC   PAHIORDR,DKHDREF+2                                               
         OC    PAHIORDR,ZEROES                                                  
         LA    R6,PAHIWKDT                                                      
         MVC   NXTSTDT,DKHDST2                                                  
         GOTO1 ADDAY,DMCB,DKHDEND1,WORK2,+1                                     
MAIN0100 CLC   WORK2(6),NXTSTDT                                                 
         BNL   MAIN0120                                                         
         MVC   0(6,R6),WORK2                                                    
         DC    H'0'                                                             
         LA    R6,6(R6)                                                         
         MVC   WORK,WORK2                                                       
         GOTO1 ADDAY,DMCB,WORK,WORK2,+7                                         
         B     MAIN0100                                                         
*                                                                               
MAIN0120 CLC   DKHDST3,SPACES                                                   
         BNH   MAIN0140                                                         
         CLC   NXTSTDT,DKHDST3                                                  
         BE    MAIN0140                                                         
         MVC   NXTSTDT,DKHDST3                                                  
         GOTO1 ADDAY,DMCB,DKHDEND2,WORK2,+1                                     
         B     MAIN0100                                                         
*                                                                               
MAIN0140 DS    0H                                                               
         DROP  R7                  DONE WITH HIATUS RECORD                      
*                                                                               
MAIN0160 DS    0H                                                               
         MVI   PAHDCTRD,C'C'                                                    
         MVI   PAHDSTYP,C'W'                                                    
         MVC   PAHDAIRD,DKHDST1                                                 
         DROP  R5                  AGENCY HEADER LINE IS UNFINISHED             
*                                                                               
         XC    BUYDTL,BUYDTL                                                    
         LA    R5,BUYDTL                                                        
         USING PBUYDTLD,R5                                                      
         MVC   PBDTSTDT,DKHDST1                                                 
         MVC   STDATE,PBDTSTDT                                                  
*                                                                               
         XC    BUYDTL2,BUYDTL2                                                  
         XC    BUYDTL3,BUYDTL3                                                  
         CLC   DKHDST2,SPACES      IS THERE A SECOND BUY DETAIL?                
         BNH   MAIN0180                                                         
         MVI   PBDTCONT,C'*'       MORE TO FOLLOW                               
         DROP  R5                  BUYDETAIL IS UNFINISHED                      
*                                                                               
         BAS   RE,BMPREC                                                        
         LA    R5,BUYDTL2                                                       
         USING PBUYDTLD,R5                                                      
         MVC   PBDTSTDT,DKHDST2                                                 
*                                                                               
         CLC   DKHDST3,SPACES                                                   
         BNH   MAIN0180                                                         
         MVI   PBDTCONT,C'*'       MORE TO FOLLOW                               
         DROP  R5                  BUYDETAIL IS UNFINISHED                      
*                                                                               
         BAS   RE,BMPREC                                                        
         LA    R5,BUYDTL3                                                       
         USING PBUYDTLD,R5                                                      
         MVC   PBDTSTDT,DKHDST3                                                 
         DROP  R4,R5               BUYDETAIL IS UNFINISHED                      
MAIN0180 B     MAIN0880                                                         
*                                                                               
MAIN0200 DS    0H                  DKHEADR2                                     
         USING DKHEADR2,R4                                                      
*                                                                               
*                                  STATION'S COMPANY CODE FROM                  
*                                     DARE STATION RECORD                       
*                                           VS                                  
*                                  OPTION CARD COMPANY                          
*                                                                               
         CLC   QOPTION1,COMPCODE   RUN FOR THIS DIVISION?                       
         BE    MAIN0212            YES - PROCESS IT                             
MAIN0204 EQU   *                                                                
         GET   INTAPE,(R4)         NO  - SKIP THIS ORDER                        
*                                  EOF --> MAIN0900                             
         CLI   DKH2TYP,DKHDTYPQ    IS RECORD A HEADER?                          
*                                     STILL 'USING' DKH2 RECD LAYOUT            
         BNE   MAIN0204            NO  - READ NEXT RECORD                       
         B     MAIN0040            YES - CHECK THIS ORDER OUT                   
MAIN0212 EQU   *                                                                
         LA    R5,AGYDS2                                                        
         USING PAGYDS2D,R5                                                      
         MVC   PAD2ESTN,DKH2EST                                                 
         MVC   PAD2TDEM+6(L'DKH2CLCD),DKH2CLCD   CLASS CODE                     
         MVC   PAD2TDEM+7(L'DKH2CSQL),DKH2CSQL   COST QUALIFIER                 
         DROP  R5                  DONE WITH AGENCY DESCRIPTION LINE 2          
*                                                                               
         MVC   AGYEDI+15(L'DKH2BFNM),DKH2BFNM  BUYING OFFICE NAME               
         MVC   AGYEDI+65(L'DKH2FLNO),DKH2FLNO  FLIGHT NUMBER                    
*                                                                               
         LA    R5,AGYDS3                                                        
         USING PAGYDS3D,R5                                                      
         MVC   PAD3PR2N(L'DKH2AGCD),DKH2AGCD                                    
         MVC   PAD3PR2N+8(L'DKH2ADCD),DKH2ADCD                                  
         MVC   PAD3PR2N+16(L'DKH2PRCD),DKH2PRCD                                 
         MVC   PAD3PR2N+32(L'DKH2TIND),DKH2TIND                                 
         MVC   DKH2DVCD,COMPCODE   SET COMPANY CODE                             
         MVC   PAD3PR2N+33(L'DKH2DVCD),DKH2DVCD                                 
         CLI   DKH2DVCD,C'S'       SELTEL?                                      
         BE    MAIN0220            YES                                          
         CLI   DKH2DVCD,C'A'       AMERICAN?                                    
         BE    MAIN0220            YES                                          
         CLI   DKH2DVCD,C'I'       NATIONAL?                                    
         BE    MAIN0220            YES                                          
         CLI   DKH2DVCD,C'C'       CONTINENTAL?                                 
         BE    MAIN0220            YES                                          
         DC    H'0'                SHOULD BE SET NOW                            
MAIN0220 EQU   *                                                                
*                                                                               
         DROP  R5                  DONE WITH AGENCY DESCRIPTION LINE 2          
         LA    R5,AGYHDR                                                        
         USING PAGYHDRD,R5         AGENCY HEADER LINE                           
*                                                                               
         CLI   DKH2DVCD,C'S'       SELTEL?                                      
         BNE   MAIN0240                                                         
         MVC   PAHDTOID(6),=C'SELTEL'                                           
         B     MAIN0320                                                         
MAIN0240 EQU   *                                                                
         CLI   DKH2DVCD,C'A'       AMERICAN?                                    
         BNE   MAIN0260                                                         
         MVC   PAHDTOID(6),=C'AMERIC'                                           
         B     MAIN0320                                                         
MAIN0260 EQU   *                                                                
         CLI   DKH2DVCD,C'C'       CONTINENTAL?                                 
         BNE   MAIN0280                                                         
         MVC   PAHDTOID(6),=C'CONTIN'                                           
         B     MAIN0320                                                         
MAIN0280 EQU   *                                                                
         CLI   DKH2DVCD,C'I'       NATIONAL?                                    
         BNE   MAIN0300                                                         
         MVC   PAHDTOID(6),=C'NATION'                                           
         B     MAIN0320                                                         
MAIN0300 EQU   *                                                                
         MVC   P+1(21),=C'UNIDENTIFIED DIVISION'                                
         MVC   P+22,DKH2DVCD                                                    
         GOTO1 REPORT                                                           
*                                                                               
         B     MAIN0204            UNIDENTIFIED:  SKIP THIS ORDER               
*                                                                               
         DC    H'0'                UNIDENTIFIED DIVISION                        
MAIN0320 EQU   *                                                                
         DROP  R5                  DONE WITH AGENCY DESCRIPTION LINE 2          
         LA    R5,AGYDS1                                                        
         USING PAGYDS1D,R5                                                      
         MVC   PAD1AGNM(L'DKH2CNAD),DKH2CNAD                                    
         MVC   PAD1AGNM+15(L'DKH2INV),DKH2INV                                   
         MVC   PAD1AGNM+30(10),DKH2BFNM                                         
         MVC   PAD1AGAD+35(L'DKH2BUOF),DKH2BUOF                                 
         DROP  R5                  DONE WITH AGENCY DESCRIPTION LINE 1          
*                                                                               
         LA    R5,AGYHDR                                                        
         USING PAGYHDRD,R5         AGENCY HEADER LINE                           
*                                                                               
         MVC   PAHDROUT(3),=C'ED1'                                              
         MVC   PAHDFRID(5),=C'LBED1'                                            
         CLC   =C'LB',DKH2AGCD       LEO BURNETT                                
         BE    MAIN0340                                                         
         MVC   PAHDROUT(3),=C'ED2'                                              
         MVC   PAHDFRID(5),=C'DMED2'                                            
         CLC   =C'DMB',DKH2AGCD    DARCY                                        
         BE    MAIN0340                                                         
         CLC   =C'HEXA',DKH2AGCD                                                
         BE    MAIN0340                                                         
         MVC   PAHDROUT(3),=C'ED3'                                              
         MVC   PAHDFRID(5),=C'MCED3'                                            
         CLC   =C'ME',DKH2AGCD     MCANN ERIKSON                                
         BE    MAIN0340                                                         
         MVC   PAHDROUT(3),=C'ED4'                                              
         MVC   PAHDFRID(5),=C'FCED4'                                            
         CLC   =C'FCB',DKH2AGCD    FOOT COMB                                    
         BE    *+6                                                              
         DC    H'0'                MUST BE ONE OF THE FOUR AGENCIES             
*                                                                               
MAIN0340 MVC   PAHDRPCN(L'DKH2CON),DKH2CON                                      
         MVC   PAHDQMED,DKH2MED                                                 
         MVC   PAHDQCLT,DKH2ADCD   CL6 <-CL8 AND CALLED ADV CODE                
         MVC   PAHDQPRD,DKH2PRCD   CL4 <-CL8                                    
         MVC   PAHDQPR2,DKH2PR2                                                 
         DROP  R5                                                               
*                                                                               
         B     MAIN0880                                                         
         DROP  R4                  AGENCY HEADER LINE STILL UNFINISHED          
*                                                                               
MAIN0360 DS    0H                  DKHEADR3                                     
         USING DKHEADR3,R4                                                      
         LA    R5,COMMREC                                                       
         USING PAGYCOMD,R5                                                      
*                                                                               
         LA    R1,DKH3COM1                                                      
*                                                                               
MAIN0380 BAS   RE,BMPREC                                                        
         MVC   PACMTID,=C'AGYCOM'                                               
         MVC   PACMORDR,DKH3REF+2                                               
         OC    PACMORDR,ZEROES                                                  
         MVC   PACMTEXT(L'DKH3COM1),0(R1)                                       
*                                                                               
         LA    R1,75(R1)           BUMP TO NEXT COMMENT FROM KATZ               
         CLC   0(L'DKH3COM1,R1),SPACES                                          
         BNH   MAIN0400                                                         
         MVI   PACMCONT,C'*'       MORE TO FOLLOW                               
         LA    R5,95(R5)           OUR NEXT COMMREC                             
         B     MAIN0380                                                         
*                                                                               
MAIN0400 B     MAIN0880                                                         
         DROP  R4,R5                                                            
*                                                                               
MAIN0420 DS    0H                  DKDETAIL                                     
         USING DKDETAIL,R4                                                      
*                                                                               
         LA    R5,BUYCOMM                                                       
         BAS   RE,BMPREC                                                        
         USING PBUYCOMD,R5                                                      
         MVC   PBCMTID,=C'BUYCOM'                                               
         MVC   PBCMORDR,DKDREF+2                                                
         OC    PBCMORDR,ZEROES                                                  
         MVC   PBCMTEXT(L'DKDCOMM),DKDCOMM                                      
         MVI   PBCMCONT,C'*'       MORE TO FOLLOW                               
         DROP  R5                                                               
*                                                                               
         OC    AGYHDR,AGYHDR                                                    
         BZ    MAIN0460                                                         
         LA    R5,AGYHDR                                                        
         USING PAGYHDRD,R5         AGENCY HEADER LINE                           
         MVC   PAHDSDAY,DKDQUAL    QUALIFIER -> OUT OF WEEK ROTATOR             
*                                                                               
*        TM    PQFLAG,X'80'        DON'T OPEN FIRST TIME                        
*        BZ    MAIN0440                                                         
*        BAS   RE,OPENPQ           OPEN PRINT QUEUE                             
*                                                                               
MAIN0440 DS    0H                                                               
*        OI    PQFLAG,X'80'        OPEN PRINT QUEUE NEXT TIME                   
         BAS   RE,MAINPRT                                                       
         DROP  R5                                                               
*                                                                               
MAIN0460 DS    0H                                                               
*                                                                               
         LA    R5,BUYHDR                                                        
         BAS   RE,BMPREC                                                        
         USING PBUYHDRD,R5                                                      
         MVC   PBHDTID,=C'BUYHDR'                                               
         MVC   PBHDORDR,DKDREF+2                                                
         OC    PBHDORDR,ZEROES                                                  
         MVC   PBHDPGNM+32(L'DKDDYPT),DKDDYPT   DAYPART                         
*                                                                               
MAIN0480 L     R1,BUYLINE                                                       
         LA    R1,1(R1)                                                         
         ST    R1,BUYLINE                                                       
         EDIT  (B4,BUYLINE),(4,PBHDBLIN),FILL=0                                 
*                                                                               
MAIN0500 MVC   PBHDROTN,DKDDAYS                                                 
         MVC   PBHDRSDT,DKDQUAL                                                 
         MVC   PBHDSTIM,DKDSTRT                                                 
         MVC   PBHDETIM,DKDEND                                                  
         MVC   PBHDTSLN,DKDLEN                                                  
         MVC   PBHDLUNT,DKDLNQL                                                 
         MVC   PBHDPGNM(L'DKDNAME),DKDNAME                                      
         MVC   PBHDCOST,DKDUNCS    UNIT COST                                    
         MVI   PBHDSTYP,C'W'                                                    
         MVC   P(L'BUYHDR),BUYHDR                                               
         BAS   RE,MYREPT                                                        
         DROP  R5                                                               
*                                                                               
         LA    R5,BUYDTL                                                        
         USING PBUYDTLD,R5                                                      
         MVC   PBDTSTDT,STDATE                                                  
         MVC   DATE,PBDTSTDT                                                    
         DROP  R5                                                               
*                                                                               
MAIN0520 LA    R5,BUYDTL                                                        
MAIN0540 DS    0H                                                               
         USING PBUYDTLD,R5                                                      
         MVC   PBDTTID,=C'BUYDTL'                                               
         MVC   PBDTORDR,DKDREF+2                                                
         OC    PBDTORDR,ZEROES                                                  
         MVC   PBDTCOST,DKDUNCS    UNIT COST                                    
*                                                                               
         OC    PBDTSTDT,PBDTSTDT                                                
         BNZ   MAIN0560                                                         
         MVC   PBDTSTDT,NXTSTDT                                                 
         B     MAIN0580                                                         
*                                                                               
MAIN0560 LA    R6,DKDSPGR          BEGINNING OF SPOT GRID                       
*                                                                               
*   TEST                                                                        
*        MVC   P+1(13),=C'GRID DISPLAY:'                                        
*        MVC   P+16(08),PBDTORDR                                                
*        MVC   P+26(28),DKDSPGR                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         CLC   DKDSPGR,ZEROES                                                   
         BNE   MAIN0600                                                         
         MVC   PBDTNOWK,ZEROES                                                  
         MVC   PBDTSPTS,ZEROES                                                  
         MVC   P(L'BUYDTL),BUYDTL                                               
         BAS   RE,MYREPT                                                        
         B     MAIN0740                                                         
*                                                                               
MAIN0580 L     R6,ASPGR            A(IN SPOT GRID)                              
MAIN0600 SR    R3,R3               NO OF WKS CTR                                
MAIN0620 LA    RE,DKDSPTS          END OF GRID REACHED?                         
         CR    R6,RE                                                            
         BNE   MAIN0630            YES                                          
*                                                                               
*   TEST                                                                        
*        MVC   P+1(9),=C'BUYDTL=  '                                             
*        MVC   P+10(L'BUYDTL),0(R5)                                             
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         B     MAIN0720            YES                                          
*                                                                               
MAIN0630 EQU   *                                                                
         OC    PBDTSPTS,PBDTSPTS   IF NO ELEM YET                               
         BZ    MAIN0640                                                         
         CLC   PBDTSPTS,ZEROES                                                  
         BNE   MAIN0660                                                         
MAIN0640 CLC   0(2,R6),ZEROES      ANY GRID ENTRY?                              
         BNE   MAIN0660            YES                                          
*                                                                               
*   TEST                                                                        
*        MVC   P+1(13),=C'GRID CYCLE  :'                                        
*        MVC   P+16(08),PBDTORDR                                                
*        MVC   P+26(2),0(R6)                                                    
*        MVC   P+30(2),PBDTSPTS                                                 
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         GOTO1 ADDAY,DMCB,DATE,NXTSTDT,+7   ...BUMP START DATE                  
         MVC   DATE,NXTSTDT                                                     
         LA    R6,2(R6)                                                         
         ST    R6,ASPGR                                                         
         XC    BUYDTL,BUYDTL                                                    
         B     MAIN0520                                                         
*                                                                               
MAIN0660 EQU   *                                                                
         OC    PBDTSPTS,PBDTSPTS   FIRST TIME?                                  
         BNZ   MAIN0670            NO                                           
         MVC   PBDTSPTS,0(R6)      MOVE IN NUMBER OF SPOTS PER WK               
MAIN0670 EQU   *                                                                
         CLC   PBDTSPTS,0(R6)      SAME # PER WK?                               
         BNE   MAIN0680                                                         
         LA    R6,2(R6)            BUMP PLACE IN SPGR                           
         LA    R3,1(R3)            # WK CTR                                     
         EDIT  (R3),(2,PBDTNOWK),FILL=0                                         
*                                  ALWAYS EDIT NUMBER OF WEEKS INTO             
*                                     RECORD AT COUNT TIME.                     
*                                  WILL BE OVERLAID BY HIGHER NUMBER            
*                                    AND WASN'T BEING INSERTED                  
*                                        WHEN ONLY GRID ENTRY WAS               
*                                        LAST ENTRY                             
         GOTO1 ADDAY,DMCB,DATE,NXTSTDT,+7   FOR LATER                           
         MVC   DATE,NXTSTDT                                                     
         B     MAIN0620                                                         
*                                                                               
MAIN0680 ST    R6,ASPGR            HOLD PLACE IN SPOT GRID                      
         MVI   PBDTCONT,C'*'                                                    
         EDIT  (R3),(2,PBDTNOWK),FILL=0                                         
         LA    R1,DKDSPTS                                                       
         SR    R1,R6                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),ZEROES                                                   
         BNE   MAIN0700                                                         
         MVI   PBDTCONT,C' '                                                    
         OI    FLAG,X'80'                                                       
*                                                                               
MAIN0700 MVC   P(L'BUYDTL),0(R5)                                                
         BAS   RE,MYREPT                                                        
         BAS   RE,BMPREC                                                        
*                                                                               
         XC    DUB,DUB                                                          
         PACK  DUB+6(2),PBDTNOWK                                                
         CVB   R1,DUB                                                           
         PACK  DUB+6(2),PBDTSPTS                                                
         CVB   RE,DUB                                                           
         STH   RE,HALF                                                          
*                                                                               
         MH    R1,HALF             MULTIPLY # SPOTS/WEEK  BY # WEEKS            
         L     RE,TOTSPOT          AND INCREMENT TOTAL                          
         AR    RE,R1                                                            
         ST    RE,TOTSPOT                                                       
*                                                                               
         PACK  DUB+3(5),PBDTCOST   COST PER SPOT                                
         CVB   RE,DUB                                                           
         ST    RE,FULL             MULTIPLY BY # OF SPOTS                       
         M     R0,FULL                                                          
         L     RE,TOTDLLRS         AND INCREMENT TOTAL                          
         AR    RE,R1                                                            
         ST    RE,TOTDLLRS                                                      
*                                                                               
         XC    BUYDTL,BUYDTL                                                    
         TM    FLAG,X'80'                                                       
         BO    MAIN0740                                                         
         B     MAIN0520                                                         
*                                                                               
MAIN0720 EQU   *                                                                
         OC    P(L'BUYDTL),P       ANY VALUE IN FIELD?                          
         BZ    MAIN0725            NO  - INSERT DETAIL                          
         CLC   P(L'BUYDTL),SPACES  ANY VALUE IN FIELD                           
         BNE   MAIN0730            YES - USE WHAT IS THERE                      
MAIN0725 EQU   *                                                                
         MVC   P(L'BUYDTL),0(R5)   NO  - MOVE IN BUYDTL                         
MAIN0730 EQU   *                                                                
         BAS   RE,MYREPT                                                        
         BAS   RE,BMPREC                                                        
*                                                                               
         XC    DUB,DUB                                                          
         PACK  DUB+6(2),PBDTNOWK                                                
         CVB   R1,DUB                                                           
         PACK  DUB+6(2),PBDTSPTS                                                
         CVB   RE,DUB                                                           
         STH   RE,HALF                                                          
*                                                                               
         MH    R1,HALF             MULTIPLY # SPOTS/WEEK  BY # WEEKS            
         L     RE,TOTSPOT          AND INCREMENT TOTAL                          
         AR    RE,R1                                                            
         ST    RE,TOTSPOT                                                       
*                                                                               
         PACK  DUB+3(5),PBDTCOST   COST PER SPOT                                
         CVB   RE,DUB                                                           
         ST    RE,FULL             MULTIPLY BY # OF SPOTS                       
         M     R0,FULL                                                          
         L     RE,TOTDLLRS         AND INCREMENT TOTAL                          
         AR    RE,R1                                                            
         ST    RE,TOTDLLRS                                                      
*                                                                               
         DROP  R5                                                               
MAIN0740 XC    FLAG,FLAG                                                        
         LA    R1,BUYDTL3                                                       
         CR    R1,R5                                                            
         BE    MAIN0780            WE ALREADY DID THE LAST BUY DETAIL           
*                                                                               
         LA    R1,BUYDTL2                                                       
         CR    R1,R5                                                            
         BE    MAIN0760            WE ALREADY DID THE SECOND BUY DETAIL         
*                                                                               
         OC    BUYDTL2,BUYDTL2                                                  
         BZ    MAIN0780            NO MORE TO DO                                
         LA    R5,BUYDTL2          DO THE SECOND                                
         B     MAIN0540                                                         
*                                                                               
MAIN0760 OC    BUYDTL3,BUYDTL3                                                  
         BZ    MAIN0780            NO MORE TO DO                                
         LA    R5,BUYDTL3          DO THE THIRD                                 
         B     MAIN0540                                                         
*                                                                               
         DROP  R4                                                               
MAIN0780 B     MAIN0880                                                         
*                                                                               
MAIN0800 DS    0H                  DKDTLCOM                                     
         USING DKDTLCOM,R4                                                      
*                                                                               
         TM    BCMFLG,X'80'                                                     
         BNZ   MAIN0820                                                         
         MVC   P(L'BUYCOMM),BUYCOMM                                             
         BAS   RE,MYREPT                                                        
         OI    BCMFLG,X'80'                                                     
*                                                                               
MAIN0820 LA    R5,P                                                             
         BAS   RE,BMPREC                                                        
         USING PBUYCOMD,R5                                                      
         MVC   PBCMTID,=C'BUYCOM'                                               
         MVC   PBCMORDR,DKDCMREF+2                                              
         OC    PBCMORDR,ZEROES                                                  
         MVC   PBCMTEXT(L'DKDCMCOM),DKDCMCOM                                    
         CLC   DKDCMCM2,SPACES                                                  
         BNH   MAIN0840                                                         
         MVI   PBCMCONT,C'*'       MORE TO FOLLOW                               
*                                                                               
         DROP  R5                                                               
         BAS   RE,BMPREC                                                        
         LA    R5,PSECOND                                                       
         USING PBUYCOMD,R5                                                      
         MVC   PBCMTID,=C'BUYCOM'                                               
         MVC   PBCMORDR,DKDCMREF+2                                              
         OC    PBCMORDR,ZEROES                                                  
         MVC   PBCMTEXT(L'DKDCMCOM),DKDCMCM2                                    
         CLC   DKDCMCM3,SPACES                                                  
         BNH   MAIN0840                                                         
         MVI   PBCMCONT,C'*'       MORE TO FOLLOW                               
*                                                                               
         DROP  R5                                                               
         LA    R5,PTHIRD                                                        
         BAS   RE,BMPREC                                                        
         USING PBUYCOMD,R5                                                      
         MVC   PBCMTID,=C'BUYCOM'                                               
         MVC   PBCMORDR,DKDCMREF+2                                              
         OC    PBCMORDR,ZEROES                                                  
         MVC   PBCMTEXT(L'DKDCMCOM),DKDCMCM3                                    
*                                                                               
MAIN0840 BAS   RE,MYREPT                                                        
         DROP  R4,R5                                                            
         B     MAIN0880                                                         
*                                                                               
MAIN0860 DS    0H                  DKTLRTLR                                     
         USING DKTLRTLR,R4                                                      
         BAS   RE,BMPREC                                                        
         LA    R5,P                                                             
         USING PAGYTLRD,R5                                                      
         MVC   PATLTID,=C'AGYTLR'                                               
         MVC   PATLORDR,DKTREF+2                                                
         OC    PATLORDR,ZEROES                                                  
         EDIT  (B4,TOTRECS),(6,PATLNMRC),FILL=0                                 
         EDIT  (B4,TOTSPOT),(6,PATLSPTS),FILL=0                                 
         EDIT  (B4,TOTDLLRS),(10,PATLTOTL),FILL=0                               
         BAS   RE,MYREPT           AGENCY TRAILER RECORD IS FINISHED            
         XC    TOTRECS,TOTRECS                                                  
         XC    TOTSPOT,TOTSPOT                                                  
         XC    TOTDLLRS,TOTDLLRS                                                
         XC    BUYLINE,BUYLINE                                                  
         DROP  R4,R5                                                            
*                                                                               
******************REMOVE********************                                    
*************************TEST****************                                   
******   B     MAINEXIT            ********                                     
******************REMOVE*********************                                   
*************************TEST*****************                                  
*                                                                               
*                                  CLOSE PRINT QUEUE                            
*        GOTO1 PRINT,DMCB,=C'CLOSE'                                             
*                                                                               
MAIN0880 GOTO1 STAPROC,DMCB,(RC)   PROCESS STATION RECORD                       
*                                                                               
         B     MAIN                GO BACK FOR NEXT RECORD                      
*                                                                               
MAIN0900 EQU   *                                                                
         CLOSE (INTAPE,REWIND)                                                  
*                                                                               
MAINEXIT XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   ACCESS DARE STATION RECORD ON CONTROL FILE.  DETERMINE AND                  
*        SET THE COMPANY CODE FOR LATER USE                                     
*                                                                               
GETCOMP  NTR1                                                                   
         L     R4,0(R1)            RESET A(INPUT RECORD)                        
         USING DKHEADR1,R4                                                      
*                                                                               
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING STAKEYD,R6                                                       
         MVI   STAKTYP,X'5A'       INSERT RECORD TYPE                           
         MVI   STAKMEDA,C'T'       INSERT MEDIA CODE                            
         MVC   STAKSTIN,DKHDSTAT   INSERT STATION LETTERS                       
         MVI   STAKSTIN+4,C'T'     INSERT 'TV'                                  
         PRINT GEN                                                              
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',WORK,AREC                     
         PRINT NOGEN                                                            
         L     R1,AREC                                                          
         MVC   WORKSTUF,WORK       SAVE FOR DISPLAY                             
         MVC   R1STUFF,0(R1)                                                    
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    GCOM0040            SAME - OKAY                                  
         DC    H'0'                DIFFERS - DUMP IT OUT                        
GCOM0040 EQU   *                                                                
         LA    R2,36(R1)           SET A(DISK ADDRESS IN KEY)                   
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFILE',(R2),AREC,      +        
               (0,DMWORK)                                                       
         L     R2,AREC                                                          
         LA    R2,42(R2)           SET TO 1ST ELEMENT IN RECORD                 
GCOM0060 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - NO REP ELEMENT                         
         CLI   0(R2),X'10'         REP ELEMENT?                                 
         BE    GCOM0080            YES - PROCESS IT                             
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF               ADD ELT LENGTH                               
         B     GCOM0060            GO BACK FOR NEXT ELEMENT                     
GCOM0080 EQU   *                                                                
         MVI   COMPCODE,C'*'       SET 'UNRECOGNIZED COMPANY'                   
         CLC   =C'KAM',2(R2)       KATZ AMERICAN?                               
         BNE   GCOM0100            NO                                           
         MVI   COMPCODE,C'A'       YES - SET COMPANY = AMERICAN                 
         B     GCOM0160                                                         
GCOM0100 EQU   *                                                                
         CLC   =C'KCO',2(R2)       KATZ CONTINENTAL?                            
         BNE   GCOM0120            NO                                           
         MVI   COMPCODE,C'C'       YES - SET COMPANY = CONTINENTAL              
         B     GCOM0160                                                         
GCOM0120 EQU   *                                                                
         CLC   =C'KNA',2(R2)       KATZ NATIONAL?                               
         BNE   GCOM0140            NO                                           
         MVI   COMPCODE,C'I'       YES - SET COMPANY = NATIONAL                 
         B     GCOM0160                                                         
GCOM0140 EQU   *                                                                
         CLC   =C'SEL',2(R2)       SELTEL?                                      
         BNE   GCOM0160            NO                                           
         MVI   COMPCODE,C'S'       YES - SET COMPANY = SELTEL                   
         B     GCOM0160                                                         
GCOM0160 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
BMPREC   NTR1                      BUMP TOTAL # OF RECORDS                      
         L     R1,TOTRECS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,TOTRECS                                                       
BMPRECX  B     MAINEXIT                                                         
*---------------------------------------------------------------------*         
* PRINT THE AGENCY HEADER, DESCRIPTIONS, COMMENTS, AND HIATUS RECS    *         
*---------------------------------------------------------------------*         
MAINPRT  NTR1                                                                   
         MVC   P(L'AGYHDR),AGYHDR                                               
         MVC   PSECOND(L'AGYDS1),AGYDS1                                         
         MVC   PTHIRD(L'AGYDS2),AGYDS2                                          
         MVC   PFOURTH(L'AGYDS3),AGYDS3                                         
         BAS   RE,MYREPT                                                        
         MVC   P(L'AGYDS4),AGYDS4                                               
         BAS   RE,MYREPT                                                        
         MVC   AGYEDI(14),AGYDS1                                                
         MVC   AGYEDI(6),=C'AGYEDI'                                             
         MVI   AGYEDI+14,C'1'                                                   
         MVC   P(L'AGYEDI),AGYEDI                                               
         BAS   RE,MYREPT                                                        
*                                                                               
         XC    AGYHDR,AGYHDR                                                    
         XC    AGYDS1,AGYDS1                                                    
         XC    AGYDS2,AGYDS2                                                    
         XC    AGYDS3,AGYDS3                                                    
         XC    AGYDS4,AGYDS4                                                    
         XC    AGYEDI,AGYEDI                                                    
*                                                                               
         CLC   HIAREC,SPACES                                                    
         BNH   *+14                                                             
         MVC   P(L'HIAREC),HIAREC                                               
         BAS   RE,MYREPT                                                        
*                                                                               
         OC    COMMREC,COMMREC                                                  
         BZ    MPXIT                                                            
         MVC   P(L'COMMREC),COMMREC                                             
         OC    COMMREC2,COMMREC2                                                
         BZ    MPXIT                                                            
         MVC   PSECOND(L'COMMREC),COMMREC2                                      
         OC    COMMREC3,COMMREC3                                                
         BZ    MPXIT                                                            
         MVC   PTHIRD(L'COMMREC),COMMREC3                                       
         OC    COMMREC4,COMMREC4                                                
         BZ    MPXIT                                                            
         MVC   PFOURTH(L'COMMREC),COMMREC4                                      
         BAS   RE,MYREPT                                                        
         OC    COMMREC5,COMMREC5                                                
         BZ    MPXIT                                                            
         MVC   P(L'COMMREC),COMMREC5                                            
         BAS   RE,MYREPT                                                        
MPXIT    EQU   *                                                                
         L     RF,RECCTR                                                        
         LA    RF,1(RF)                                                         
         ST    RF,RECCTR                                                        
         B     MAINEXIT                                                         
******************************************************************              
*  OPENS THE PRINT QUEUE                                                        
******************************************************************              
OPENPQ   NTR1                                                                   
         L     R8,REMOTEC                                                       
         USING REMOTED,R8                                                       
*                                                                               
         MVC   REMOTAOP,=V(PQOPEN)                                              
         MVC   REMOTABF,=V(PQBUFF)                                              
         MVC   REMOTADM,=V(DATAMGR)                                             
*        MVC   REMOTDST,=X'2406'   ID FOR EJOR                                  
         MVC   REMOTDST,=X'3580'   ID FOR TTV1                                  
         MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(3),=C'KUI'                                              
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTLPP,68                                                      
         MVI   REMOTCLS,C'N'                                                    
         MVC   REMOTJID,=C'KUI'                                                 
*                                                                               
         B     MAINEXIT                                                         
         DROP  R8                                                               
         EJECT                                                                  
******************************************************************              
*  FORCE CONTINUOUS RUN             NO PAGING                                   
******************************************************************              
MYREPT   NTR1                                                                   
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVC   PTHIRD,SPACES                                                    
         MVC   PFOURTH,SPACES                                                   
         B     MAINEXIT                                                         
******************************************************************              
*  STAPROC:  CONVERT EDI RECORD TO DARE FORMAT                   *              
******************************************************************              
*                                                                               
STAPROC  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,ARECAREA         SET A(INPUT RECORD)                          
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
INITIAL  NTR1                                                                   
         OPEN  (INTAPE,(INPUT))                                                 
         SPACE 1                                                                
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RE,VUTL                                                          
         ST    RE,UTLADDR                                                       
         MVC   UTLSTUFF,0(RE)      SAVE UTLSTUFF FOR DISPLAY                    
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',100000,100000                                   
*                                  GET 100K STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
         L     RF,ARECAREA                                                      
         A     RF,=F'40000'        TAPE BUFFER AREA:                            
*                                                                               
         CLI   QOPTION1,C' '       SELTEL?                                      
         BE    INIT0040            YES                                          
         CLI   QOPTION1,X'00'      SELTEL?                                      
         BE    INIT0040            YES                                          
         CLI   QOPTION1,C'S'       SELTEL?                                      
         BNE   INIT0080                                                         
INIT0040 EQU   *                                                                
         MVI   QOPTION1,C'S'       SET 'SELTEL'                                 
INIT0080 EQU   *                                                                
*                                                                               
*   OPEN CONTROL SYSTEM TO ACCESS CONTROL FILE DARE STATION RECORDS             
*                                                                               
         PRINT GEN                                                              
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',AREC,0                                             
         L     RF,UTLADDR          LOAD A(UTL)                                  
         MVI   4(RF),X'0A'         RESET UTL TO CONTROL FILE                    
*  OPEN GEN FILE                                                                
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'CONTROL',FLIST,AREC                   
         SPACE 1                                                                
         PRINT NOGEN                                                            
         XIT1                                                                   
FLIST    DS    0H                                                               
         DC    CL8'NGENFILE'                                                    
         DC    CL8' GENDIR '                                                    
         DC    CL8'X       '                                                    
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
*    WORK SPACE, ETC.                                                           
         SPACE 2                                                                
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
UTLADDR  DS    A                                                                
WORK2    DS    CL6                                                              
NXTSTDT  DS    CL6                                                              
STDATE   DS    CL6                                                              
DATE     DS    CL6                                                              
AGYHDR   DS    CL130                                                            
AGYEDI   DS    CL95                                                             
AGYDS1   DS    CL95                                                             
AGYDS2   DS    CL90                                                             
AGYDS3   DS    CL85                                                             
AGYDS4   DS    CL60                                                             
HIAREC   DS    CL75                AREA FOR AGENCY HIATUS WEEK LINE REC         
COMMREC  DS    CL95                AREA FOR COMMENT LINE 1                      
COMMREC2 DS    CL95                AREA FOR COMMENT LINE 2                      
COMMREC3 DS    CL95                AREA FOR COMMENT LINE 3                      
COMMREC4 DS    CL95                AREA FOR COMMENT LINE 4                      
COMMREC5 DS    CL95                AREA FOR COMMENT LINE 5                      
BUYCOMM  DS    CL95                                                             
BUYHDR   DS    CL90                                                             
BUYDTL   DS    CL35                AREA FOR FIRST BUY DETAIL                    
BUYDTL2  DS    CL35                AREA FOR SECOND BUY DETAIL                   
BUYDTL3  DS    CL35                AREA FOR THIRD BUY DETAIL                    
ASPGR    DS    F                   ADDRESS OF WHERE WE ARE IN SPOT GRID         
*                                                                               
TOTRECS  DS    F                   FOR TRAILER REC                              
TOTSPOT  DS    F                   FOR TRAILER REC                              
TOTDLLRS DS    F                   FOR TRAILER REC                              
BUYLINE  DS    F                   FOR BUYLINE NUMBER                           
RECCTR   DS    F                                                                
AREC     DC    A(REC)                                                           
*                                                                               
FLAG     DS    X                                                                
PQFLAG   DS    X                   X'80' = OPEN PRINT QUEUE                     
SAVID    DS    XL2                                                              
ZEROES   DC    C'0000000000000000000000000000'                                  
BCMFLG   DS    X                                                                
COMPCODE DS    C                   COMPANY CODE FROM CONTROL FILE               
RKEY     DS    0XL27                                                            
UTLSTUFF DS    CL5                                                              
KEYSTUFF DC    C'*STUFF*'                                                       
WORKSTUF DS    CL50                                                             
R1STUFF  DS    CL50                                                             
REC      DS    2058X                                                            
         EJECT                                                                  
*                                                                               
ELCODE   DS    CL1                                                              
         SPACE 3                                                                
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=FB,LRECL=529,              X        
               BLKSIZE=10580,MACRF=GM,EODAD=MAIN0900                            
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(REED02,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  INCLUDE REGENSTA                STATION     RECORD                           
*  INCLUDE DMPRTQL                                                              
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
RECD     DSECT                                                                  
RECORD   DS    CL1008                                                           
         ORG   RECORD                                                           
       ++INCLUDE REGENSTA          STATION     RECORD                           
         EJECT                                                                  
         ORG   RECORD                                                           
       ++INCLUDE REGENMKT          MARKET      RECORD                           
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE DRKZEDIIND                                                     
         EJECT                                                                  
       ++INCLUDE CTGENSTAD                                                      
         EJECT                                                                  
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030REREPED02D06/19/97'                                      
         END                                                                    

*          DATA SET SPREPL702  AT LEVEL 033 AS OF 08/10/05                      
*PHASE SPL702A                                                                  
         TITLE 'SPL702 - GLOBAL OVERRIDE REPORT - CANADA'                       
SPL702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPL702,R7                                                      
         SPACE 1                                                                
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         L     R8,ADBUY                                                         
         USING DOVRECD,R8                                                       
         SPACE 2                                                                
         CLI   MODE,REQFRST                                                     
         BE    HEDHOOKR                                                         
         EJECT                                                                  
EXIT     DS    0H                                                               
         XIT1                                                                   
         SPACE 1                                                                
HEDHOOKR DS    0H                                                               
         STM   R8,RC,HDHKR8                                                     
         SPACE                                                                  
         BAS   RE,GETNWK           GET NETWORK RECS                             
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
         XC    KEY,KEY             SET TO 0                                     
         MVC   KEY(2),=X'0D17'     SET UP THE KEY                               
         MVC   KEY+2(1),BAGYMD     AGENCY/MEDIA                                 
         CLC   =C'ALL',QNETWRK     CHECK IF ALL                                 
         BE    FO1                 NO -                                         
         MVC   MYMKT,=C'0000'      GET NETWORK STATION NUMBER                   
         MVC   MYSTA(4),QNETWRK                                                 
         OC    MYSTA,=C'    '                                                   
         MVI   MYSTA+4,C' '                                                     
         GOTO1 MSPACK,DMCB,MYMKT,MYSTA,MYBMKST                                  
         MVC   KEY+3(2),MYBMKST+2                                               
FO1      CLC   =C'ALL',QCLT        CHECK IF ALL CLIENTS                         
         BE    FO2                                                              
         GOTO1 CLPACK,DMCB,QCLT,KEY+5                                           
         SPACE 1                                                                
FO2      CLC   =C'ALL',QSHOW       CHECK IF ALL                                 
         BE    FO3                                                              
         MVC   KEY+7(4),QSHOW                                                   
FO3      GOTO1 HIGH                                                             
         B     GR000                                                            
         SPACE 1                                                                
NEXTREC  DS    0H                                                               
         MVI   FORCEHED,C'Y'       FORM FEED                                    
         GOTO1 SEQ                 GET THE NEXT RECORD                          
         EJECT                                                                  
*        ***   GET THE RECORD      ***                                          
GR000    DS    0H                                                               
         CLC   =C'ALL',QNETWRK                                                  
         BNE   GR010                                                            
         CLC   KEY(3),KEYSAVE                                                   
         BNE   DONE                ALL NETWORKS                                 
         CLC   =C'ALL',QSHOW                                                    
         BE    GR05                                                             
         CLC   KEY+7(4),KEYSAVE+7  SPECIFIC SHOW                                
         BNE   NEXTREC                                                          
*                                                                               
GR05     CLC   =C'ALL',QCLT        ALL CLIENTS                                  
         BE    GR030                                                            
         CLC   KEY+5(2),KEYSAVE+5  SPECIFIC CLIENT                              
         BNE   NEXTREC                                                          
         B     GR030                                                            
*                                                                               
GR010    CLC   =C'ALL',QCLT        ALL CLIENTS                                  
         BNE   GR015                                                            
         CLC   KEY(5),KEYSAVE                                                   
         BNE   DONE                                                             
         CLC   =C'ALL',QSHOW       ALL CLIENTS ALL SHOWS                        
         BE    GR030                                                            
         CLC   KEY+7(4),KEYSAVE+7  ALL CLIENTS - SPECIFIC SHOW                  
         BNE   NEXTREC                                                          
         B     GR030                                                            
*                                                                               
GR015    CLC   =C'ALL',QSHOW                                                    
         BNE   GR020                                                            
         CLC   KEY(7),KEYSAVE      SPECIFIC CLT - ALL SHOW                      
         BNE   DONE                                                             
         B     GR030                                                            
*                                                                               
GR020    CLC   KEY(11),KEYSAVE     SPECIFIC CLT & SHOW                          
         BNE   DONE                                                             
*                                                                               
GR030    MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
         EJECT                                                                  
*        ***   WRITE OUT HEADERS   ***                                          
         MVC   P+3(5),=C'SHOW='                                                 
         MVC   BBKWORK,DOVBBK      RIGHT PADDING WITH '00'                      
         MVC   UTBKWORK,DOVUTBK    RIGHT PADDING WITH '00'                      
*                                                                               
         XC    MYBMKST,MYBMKST                                                  
         MVC   MYBMKST+2(2),DOVKNET (MARKET IS NULL = NETWORK)                  
         GOTO1 MSUNPK,DMCB,MYBMKST,MYMKT,THISNET NETWORK (EBCDIC)               
         MVC   P+10(4),THISNET                                                  
*                                                                               
         LA    R4,P+13                                                          
         CLI   0(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         MVI   1(R4),C'/'                                                       
         MVC   2(4,R4),DOVKPGM                                                  
         MVI   6(R4),C'/'                                                       
         CLI   DOVKRTS,C'0'                                                     
         BNE   HD010                                                            
         MVC   7(3,R4),=C'NSI'                                                  
         B     HD020                                                            
HD010    DS    0H                                                               
         MVC   7(3,R4),=C'BBM'                                                  
HD020    DS    0H                                                               
         CLI   DOVKSEQ,0           SEE IF THERE IS A SEQ NUMBER                 
         BE    *+10                NO                                           
         MVC   10(2,R4),=C',1'     MUST BE 1                                    
         OC    DOVKCLT,DOVKCLT     SEE IF THERE'S A CLIENT                      
         BZ    HD030                                                            
         LA    R1,11(R4)                                                        
         SPACE 1                                                                
HD025    CLI   0(R1),C' '                                                       
         BNE   HD027                                                            
         BCTR  R1,0                                                             
         B     HD025                                                            
HD027    LA    R4,1(R1)                                                         
         MVI   0(R4),C'/'                                                       
         GOTO1 CLUNPK,DMCB,DOVKCLT,WORK                                         
         MVC   1(3,R4),WORK                                                     
         SPACE 1                                                                
HD030    MVC   P+28(9),=C'BASE BOOK'                                            
         GOTO1 DATCON,DMCB,(3,DOVBBK2),(9,P+38) BASE BOOK                       
         MVC   P+50(13),=C'USE TILL BOOK'                                       
         GOTO1 DATCON,DMCB,(3,DOVUTBK2),(9,P+65) USE TILL BOOK                  
         CLC   DOVADAT,=X'000000'  CHECK IF EMPTY                               
         BE    HD040               YES, SO BRANCH PAST THIS MSG.                
         MVC   P+75(18),=C'LAST ACTIVITY DATE'                                  
         GOTO1 DATCON,DMCB,(3,DOVADAT),(5,P+98) LAST ACT. DATE                  
HD040    GOTO1 REPORT              PRINT BOOK/BOOK/ACT. DATE                    
         BAS   RE,SHOWNAME      GET SHOW NAME AND PUT IN PRINT LINE             
HD045    GOTO1 REPORT              PRINT A BLANK LINE                           
*                                                                               
         LA    R3,24(R8)           CHECK FOR A REFERRAL ELEMENT                 
HD050    ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    HD060                                                            
         CLI   0(R3),99                                                         
         BNE   HD050                                                            
         MVC   P+25(28),=C'*** DEMOS FROM SHOW XXXX ***'                        
         MVC   P+45(4),2(R3)                                                    
         GOTO1 REPORT                                                           
         B     NEXTREC                                                          
*                                                                               
HD060    LA    R3,24(R8)           GET ADDR FOR ELEM AFTER KEY                  
*                                                                               
SHIFTREC DS    0H                                                               
         ZIC   R6,1(R3)            GET ELEMENT LENGTH FOR SHIFT                 
         AR    R3,R6               MOVE TO NEXT ELEMENT                         
         CLI   0(R3),2             IS IT AN '02' ELEMENT                        
         BNE   L1L010              NO, SO GO ON TO '01' ELEMENT LOOP            
L2L010   DS    0H                                                               
         USING DOVEL02,R3                                                       
         ZIC   R5,1(R3)            GET ELEMENT LENGTH                           
         SH    R5,=H'2'                                                         
         BZ    DONE                JUST IN CASE                                 
         SR    R4,R4                                                            
         D     R4,=F'5'                                                         
         LA    R6,DOVIMPC                                                       
         MVC   P1+3(16),=C'DEMO IMPRESSIONS'                                    
L2L020   DS    0H                                                               
         LA    R4,P1+25                                                         
         LA    R2,4                                                             
         CR    R5,R2                                                            
         SR    R5,R2               DEDUCT REMAINING ELEMENTS                    
         LTR   R5,R5               CHECK IF R5>4                                
         BP    L2L030                                                           
         AR    R2,R5                                                            
L2L030   DS    0H                  LOOP TO GET 4 DOV'S IN A LINE                
* GET DEMO NAME *******                                                         
         SPACE 1                                                                
         L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
         XC    0(256,RE),0(RE)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
         GOTO1 DEMOCON,DMCB,(1,0(R6)),(2,0(R4)),(C'S',ADBLOCK),        X        
               (SPOTPROF+9,0)                                                   
         MVI   8(R4),C'='          PUT IN AN EQUALS SIGN                        
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),3(R6)                                                  
         EDIT  (B4,FULL),(6,10(R4)),1                                           
         LA    R4,20(R4)           MOVE OVER ON PRINTLINE                       
         LA    R6,5(R6)            MOVE POINTER DOWN GLOBAL OVD. LIST           
         BCT   R2,L2L030                                                        
         GOTO1 REPORT              PRINT DEMO IMPRESSIONS                       
         LTR   R5,R5                                                            
         BP    L2L020                                                           
L1L010   DS    0H                                                               
         GOTO1 REPORT              PRINT A BLANK LINE                           
         LA    R0,SSHDHK           ENABLE HEADHOOK ROUTINES                     
         ST    R0,HEADHOOK                                                      
         XC    SUBHEAD1,SUBHEAD1   CLEAR HEADHOOK LINES                         
         XC    SUBHEAD2,SUBHEAD2                                                
         LA    R3,24(R8)           POINT TO BEGINNING OF '01' ELEMENT           
         USING DOVRECD,R8          BACK TO '01' ELEMENT                         
         CLI   0(R3),1             IS IT THE EXPECTED '01' ELEMENT              
         BNE   ERR1                IF NOT THEN GOTO ERROR ROUTINE 1             
         ZIC   R5,1(R3)            GET ELEMENT LENGTH                           
         S     R5,=F'12'                                                        
         SR    R4,R4                                                            
         D     R4,=F'3'            END OF ELEMENT GETTING                       
         C     R5,=F'10'           CHECK IF MORE THAN 10 DEMOS                  
         BL    L1L020                   FROM OLD INPUT PARMS                    
         L     R5,=F'10'           SET TO 10 IF MORE THAN 10                    
L1L020   LA    R4,P1                                                            
         STC   R5,NUMDEMOS                                                      
         MVC   3(5,R4),=C'DEMO='                                                
         MVC   135(5,R4),=C'-----'                                              
         LA    R6,DOVDLSTC         DEMO LIST LOCATION                           
L1L030   DS    0H                                                               
         LA    R4,10(R4)           LINE DISPLACEMENT LENGTH                     
         SPACE 1                                                                
* GET DEMO NAME *******                                                         
         L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
         XC    0(256,RE),0(RE)                                                  
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'C'                                                    
         DROP  RE                                                               
         GOTO1 DEMOCON,DMCB,(1,0(R6)),(2,0(R4)),(C'S',ADBLOCK),        X        
               (SPOTPROF+9,0)                                                   
         SPACE 1                                                                
         MVC   132(7,R4),=C'-------'                                            
         LA    R6,3(R6)            MOVE POINTER 3 BYTES DOWN DEMO LIST          
*                                                                               
         OC    0(3,R6),0(R6)       ANY MORE DEMOS                               
         BZ    *+8                                                              
         BCT   R5,L1L030                                                        
         CHI   R5,0                                                             
         BNH   L1L040                                                           
         ZIC   R1,NUMDEMOS                                                      
         SR    R1,R5                                                            
         LA    R1,1(R1)                                                         
         STC   R1,NUMDEMOS                                                      
*                                                                               
L1L040   MVC   SUBHEAD1,P1         PUT LINES IN SUBHEAD                         
         MVC   SUBHEAD2,P2         FOR HEADHOOK ROUTINE                         
         GOTO1 REPORT                                                           
         GOTO1 REPORT              PRINT A BLANK LINE                           
         LA    R3,24(R8)           SET R3 BACK TO '01' TO FIND '05'             
L5L010   DS    0H                                                               
         MVI   ELCODE,5            ELEMENT NUMBER IS '05'                       
         BAS   RE,NXTL             ROUTINE TO GET '05' ELEMENT                  
         BNE   PRINTIT             ELEMENT '05' NOT FOUND                       
L5L020   DS    0H                                                               
         USING DOVEL05,R3                                                       
         ZIC   R5,1(R3)            GET ELEMENT LENGTH                           
         S     R5,=F'5'                                                         
         SR    R4,R4                                                            
         D     R4,=F'2'            END OF GETTING ELEMENT LENGTH                
         C     R5,=F'10'           CHECK IF MORE THAN 10 VALUES                 
         BL    L5L021                   IN LIST FROM OLD INPUT PARMS            
         L     R5,=F'10'           SET TO 10 IF MORE THAN 10                    
L5L021   DS    0H                                                               
         ZIC   R5,NUMDEMOS         <- REAL NUMBER OF DEMOS                      
         LA    R6,DOVDEMV                                                       
         MVC   STADAT1,DOVSTA      PUT IN STADAT1 FOR BREAKDOWN                 
         CLI   STADAT2,X'00'       CHECK IF A SPILL MARKET                      
         BNE   L5L022              NO IT IS NOT                                 
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,STADAT3        MAKE SURE ITS A MARKET                       
         CHI   RE,9999                                                          
         BH    L5L022                                                           
*                                                                               
         LA    R4,132(R2)          KEEP TOGETHER SPILL & STA.                   
         LA    R2,132(R2)          OFFSET R2 FOR ANOTHER LINE                   
         EDIT  (B2,STADAT3),(5,2(R4)) CONVERT TO PRINTABLE                      
         B     L5L030              SKIP THE MSUNPK                              
L5L022   DS    0H                                                               
         LA    R4,P1+2 +2 FOR ALIGNMENT WITH '01' LIST                          
         LR    R2,R4               RESET R2 TO P1                               
         GOTO1 MSUNPK,DMCB,(X'80',STATN1),MYMKT,MYSTAL                          
         LA    RF,NETTAB           DETERMINE STATUS OF NETWORK                  
         CLC   0(4,RF),THISNET                                                  
         BE    *+12                                                             
         LA    RF,L'NETTAB(RF)                                                  
         B     *-14                                                             
         CLI   4(RF),NDEFCABQ      SPECIALTY CABLE - SHOW /SUFFIX               
         BE    *+14                                                             
         MVC   1(4,R4),MYSTA                                                    
         B     L5L030                                                           
         MVC   1(3,R4),MYSTA+4                                                  
L5L030   DS    0H                                                               
         LA    R4,10(R4) DISPLACEMENT WITHIN PRINTLINE                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,0(R6)                                                       
         BNZ   L5L032                                                           
         MVI   4(R4),C'L'          ZERO MEANS LOOKUP                            
         B     L5L034                                                           
*                                                                               
L5L032   C     R0,=X'00008000'     8000 MEANS ZERO                              
         BNE   *+6                                                              
         SR    R0,R0                                                            
         EDIT  (R0),(5,0(R4)),1,ZERO=NOBLANK                                    
*                                                                               
L5L034   LA    R6,2(R6)            SHIFT OVER THE DOVDEMV MARKER                
         BCT   R5,L5L030                                                        
*                                                                               
         ZIC   R5,1(R3)            GET LENGTH OF ELEMENT                        
         AR    R5,R3               ADD TO ADDRESS TO GET NEXT                   
         CLI   0(R5),0             CHECK IF NEXT ELEMENT EXISTS                 
         BE    L5L040              NO, SO PRINT WHAT IS IN P-LINES              
         CLI   2(R5),X'00'         CHECK IF A SPILL MARKET                      
         BE    L5L010              YES, SO GO TO NEXT ELEMENT W/O PRINT         
L5L040   GOTO1 REPORT                                                           
         B     L5L010                                                           
PRINTIT  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R0,R0               DISABLE HEADHOOK BECAUSE NEW RECORD          
         ST    R0,HEADHOOK         HAS A DIFFERENT DEMO LIST                    
         B     NEXTREC             GO TO NEXT RECORD                            
         EJECT                                                                  
*                                                                               
*        GET NETWORK RECORDS                                                    
*                                                                               
GETNWK   NTR1                                                                   
         MVC   SVGLKEY,KEY         SAVE KEY                                     
         LA    R4,NETTAB                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(2),AGY                                                     
         CLC   =C'ALL',QNETWRK     CHECK IF ALL                                 
         BE    *+10                                                             
         MVC   KEY+4(4),QNETWRK                                                 
*                                                                               
GN10     GOTO1 HIGH                                                             
*                                                                               
GN20     LA    R1,3                                                             
         CLC   =C'ALL',QNETWRK     CHECK IF ALL                                 
         BE    *+8                                                              
         LA    R1,12                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   GETX                                                             
*                                                                               
         L     R8,ADGOAL                                                        
         ST    R8,AREC                                                          
         USING NDEFRECD,R8                                                      
         GOTO1 GET                                                              
         LA    R3,NDEFEL                                                        
         MVI   ELCODE,X'02'        FIND NETWORK SEQUENCE NUMBER                 
         USING NDEFEL02,R3                                                      
         BAS   RE,GETL             ROUTINE TO GET ELEMENT                       
*                                                                               
GN40     CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                TABLE TOO SMALL                              
         MVC   0(4,R4),KEY+4       NETWORK                                      
         MVC   4(1,R4),NDEFNET     NETWORK SEQUENCE NUMBER                      
         LA    R4,L'NETTAB(R4)                                                  
         MVC   KEY+8(5),=5X'FF'    SKIP CLIENT/EST LEVEL RECS                   
         B     GN10                                                             
*                                                                               
GETX     MVC   KEY,SVGLKEY         RESTORE KEY                                  
         B     EXIT                                                             
         EJECT                                                                  
ERR1     DC    H'0'                MAKE THE PROGRAM BLOW UP                     
ERR2     DC    H'0'                MAKE THE PROGRAM BLOW UP                     
NXTL     DS    0H                                                               
         CLI   0(R3),0             CHECK IF NEXT ELEMENT EXISTS                 
         BE    NXTLX               NO, SO EXIT                                  
         SR    R0,R0                                                            
         IC    R0,1(R3)            GET ELEMENT LENGTH                           
         AR    R3,R0                                                            
GETL     CLC   ELCODE,0(R3)        IS NEXT ELEMENT WHAT WE WANT                 
         BER   RE                                                               
         B     NXTL                                                             
NXTLX    DS    0H                                                               
         LTR   RE,RE               SET CONDITION CODE                           
         BR    RE                                                               
         EJECT                                                                  
*        ***   HEADHOOK ROUTINES   ***                                          
         DROP  RB                                                               
         DS    0H                                                               
         USING *,RF                                                             
SSHDHK   NTR1                                                                   
         LM    R8,RC,HDHKR8                                                     
         B     HDHK0                                                            
HDHKR8   DC    5F'0'                                                            
         DROP  RF                                                               
         USING SPL702,RB                                                        
HDHK0    DS    0H                                                               
         MVC   MID1,SUBHEAD1       MOVE SAVED '01' LINE INTO                    
         MVC   MID2,SUBHEAD2       MIDLINES FOR HEADHOOK                        
         MVI   FORCEMID,C'Y'       ENABLE MIDLINES                              
         B     EXIT                                                             
DONE     DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*********************************************************************           
*          GET FULL SHOW NAME                                       *           
*********************************************************************           
SHOWNAME NTR1                                                                   
         MVC   MYKEY,KEY                                                        
         LA    R6,MYKEY                                                         
         USING DOVRECD,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING NPGMRECD,R5                                                      
*                                                                               
         MVC   NPGMKTYP,=X'0D12'   TYPE                                         
         MVC   NPGMKAGY,AGY                                                     
                                                                                
* --------- GET NETWORK NAME --------------------->                             
                                                                                
         XC    MYBMKST,MYBMKST                                                  
         MVC   MYBMKST+2(2),DOVKNET (MARKET IS NULL = NETWORK)                  
         GOTO1 MSUNPK,DMCB,MYBMKST,MYMKT,MYSTA  NETWORK (EBCDIC)                
         MVC   NPGMKNET,MYSTA      NETWORK                                      
                                                                                
* ---------------------------------------------------->                         
                                                                                
         MVC   NPGMKID,DOVKPGM     SHOW ID                                      
         DROP  R5,R6                                                            
         MVC   P+3(5),=C'NAME='                                                 
         MVC   P+10(7),=C'UNKNOWN'                                              
         GOTO1 HIGH                                                             
         CLC   KEY(L'NPGMKEY),KEYSAVE                                           
         BNE   SNEX                                                             
*                                                                               
         L     R0,AREC             SAVE IO                                      
         L     R5,=A(MYIO)                                                      
         ST    R5,AREC                                                          
         USING NPGMRECD,R5                                                      
*                                                                               
         GOTO1 GET                                                              
         OC    NPGMPGM,SPACES                                                   
         MVC   P+10(17),NPGMPGM                                                 
*                                                                               
         GOTO1 CODAY,DMCB,NPGMDAY,P+30                                          
         GOTO1 UNTIME,DMCB,NPGMSTR,P+36                                         
         DROP  R5                                                               
*                                                                               
SNEX     ST    R0,AREC             RESTORE IO                                   
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                                                             
         XIT1                                                                   
********************************************************************            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* DEFINE CONSTANTS AND DSECTS AND VARIABLES                                     
STATN1   DS    0XL5                CONVETRS STATION DATA INTO                   
         DC    XL2'0000'                INDIVIDUALLY ACCESSIBLE                 
STADAT1  DS    0XL3                     LABELLED STORAGE LOCATIONS              
STADAT2  DS    XL1                                                              
STADAT3  DS    XL2                                                              
*                                                                               
MYBMKST  DS    CL5                 NEEDED FOR MSUNPK                            
MYMKT    DS    CL4                 NEEDED FOR MSUNPK/MSPACK                     
MYSTAL   DS    0CL8                NEEDED FOR MSUNPK/MSPACK                     
MYSTA    DS    CL5                 NEEDED FOR MSUNPK/MSPACK                     
MYSTAX   DS    CL3                 NEEDED FOR MSUNPK/MSPACK                     
THISNET  DS    CL5                                                              
*                                                                               
ELCODE   DS    CL1                 CODE FOR NXTL ROUTINE TO LOOK FOR            
DOVBBK2  DS    0XL3                RIGHT PADDING OF '00' FOR DOVBBK             
BBKWORK  DS    XL2                                                              
         DC    XL1'00'                                                          
DOVUTBK2 DS    0XL3                RIGHT PADDING OF '00' FOR DOVUTBK            
UTBKWORK DS    XL2                                                              
         DC    XL1'00'                                                          
SUBHEAD1 DS    CL132               STORAGE FOR HEADHOOK LINES                   
SUBHEAD2 DS    CL132                                                            
*                                                                               
NUMDEMOS DS    XL1                                                              
*                                                                               
MYKEY    DS    CL42                                                             
MYIO     DS    2000C                                                            
NETTAB   DS    2000CL5                                                          
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE SPGENDOV                                                       
         EJECT                                                                  
       ++INCLUDE SPGENNDEF                                                      
         EJECT                                                                  
       ++INCLUDE SPGENNPGM                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
QNETWRK  EQU   QMKT                                                             
QSHOW    EQU   QSTA                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033SPREPL702 08/10/05'                                      
         END                                                                    

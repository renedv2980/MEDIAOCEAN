*          DATA SET DEMVSCAN   AT LEVEL 001 AS OF 03/30/17                      
*PHASE DEMVSCNA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*ENTRY   E35                                                                    
         TITLE 'READ FILE OF KEYS, READ FROM VSAM AND OUTPUT TO FILE'           
*                                                                               
* PROGRAM CAN RUN AS AN E35 SORT EXIT IN WHICH CASE PARAMETER CARDS             
* ARE READ FROM D2VIN RATHER THAN SYSIN. IF D2VIN CAN'T BE OPENED, WE           
* ASSUME NO PARAMETER CARDS.                                                    
*                                                                               
DEMVSCAN CSECT                                                                  
         ENTRY E35                                                              
         PRINT NOGEN                                                            
         NBASE 0,**DVSC**,RA,WORK=V(REGSAVE)                                    
         MVI   E35EXIT,C'N'                                                     
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     L     R9,VCPRINT                                                       
         USING DPRINT,R9           R9=A(PRINT CSECT)                            
         LARL  R3,OUTAREA                                                       
         USING OTREC,R3            R3=A(OUTPUT RECORD)                          
         USING DVREC,OTVSREC       VSAM RECORD IN OUTPUT RECORD                 
         ZAP   PAGE,=P'1'                                                       
         ZAP   LINE,=P'99'                                                      
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(19),=C'DEMO VSAM SCAN. E35' ASSUME E35                     
*                                                                               
         CLI   E35EXIT,C'Y'        TEST RUNNING AS A SORT E35 EXIT              
         JNE   INIT10                                                           
         L     RE,=V(SYSIN)        PICK UP SYSIN DCB IN CARDS                   
         MVC   40(8,RE),=CL8'D2VIN' CHANGE DDNAME                               
         BRAS  RE,VALPARMS         VALIDATE PARAMETER CARD(S)                   
         J     INIT20              OPEN VSAM FILE NEEDED                        
*                                                                               
INIT10   STCM  R3,7,DSNXTRCT+1     EXTRACT INPUT DSN INTO TITLE                 
         LA    R1,DSNXTRCT                                                      
         LARL  RF,IFILE                                                         
         STCM  R1,7,X'25'(RF)      SET EXLST ADDRESS IN DCB                     
         RDJFCB ((RF))                                                          
         MVC   TITLE+16(4),=C'DSN='                                             
         MVC   TITLE+20(44),0(R3)                                               
*                                                                               
         OPEN  (IFILE,INPUT,OFILE,OUTPUT)                                       
*                                                                               
         BRAS  RE,VALPARMS         VALIDATE PARAMETER CARD(S)                   
*                                                                               
INIT20   OPEN  (DEMACB)            OPEN VSAM                                    
         LTR   RF,RF               RF HOLDS ERROR RETURN CODE                   
         JNZ   *+2                                                              
         CLI   SPLIT,C'Y'          SPLIT FILE?                                  
         JNE   INITX               NO                                           
         OPEN  (DEMAC2)                                                         
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
INITX    ZAP   LINE,=P'99'                                                      
         MVC   MID1,SPACES                                                      
         MVC   MID1(15),=C'ERRORS/WARNINGS'                                     
         MVC   MID2,SPACES                                                      
         MVC   MID2(15),=C'---------------'                                     
         EJECT                                                                  
***********************************************************************         
* MAIN LOOP                                                           *         
***********************************************************************         
         USING INREC,R2                                                         
MAIN     CLI   E35EXIT,C'Y'        TEST RUNNING AS A SORT E35 EXIT              
         JNE   MAIN02                                                           
         LTR   R2,R2               YES, R2 IS ALREADY A(RECORD)                 
         JNZ   MAIN10                                                           
         J     EOFIN               OR R2 IS ZERO IF NO MORE RECORDS             
MAIN02   LARL  R1,IFILE                                                         
         GET   (1)                 GET NEXT RECORD                              
         LR    R2,R1                                                            
MAIN10   AP    CTRIN,=P'1'         COUNT RECORDS IN                             
*                                                                               
         CLI   INPARM,C'V'         INPUT=VSAM?                                  
         JNE   MAIN12              NO, USE DANDX KEYS                           
         MVC   VSMKEY,INKEY        VSAM KEY                                     
         MVI   KEYLEN,L'VSMKEY     KEY LENGTH                                   
         J     MAIN20                                                           
MAIN12   MVI   VSMKSEQ,0           DANDX KEY HAS NO SEQ                         
         MVC   VSMKDND,VSMKEY      COPY MAJ + MIN                               
         MVI   KEYLEN,L'VSMKDND    KEY LENGTH                                   
         CLC   INLENMVS,=Y(INPLENQ) IS THIS A DIRECTORY RECORD?                 
         JNE   MAIN20              NO, SKIP                                     
         XC    VSMKMIN,VSMKMIN     YES, IT HAS NO MINOR KEY                     
         MVI   KEYLEN,L'VSMKMAJ    KEY LENGTH                                   
*                                                                               
         USING IFGRPL,R5                                                        
MAIN20   LARL  R5,DEMRPL                                                        
         CLI   SPLIT,C'Y'          SPLIT FILE?                                  
         JNE   MAIN22              NO                                           
         CLC   INKEY(3),=C'RTN'    'RTN' RECORDS ARE HALF OF DEMVSMN            
         JNE   MAIN22              AND FORM THE SECOND HALF OF SPLIT            
         LARL  R5,DEMRP2                                                        
MAIN22   NI    RPLOPT1,255-RPLSEQ                                               
         OI    RPLOPT1,RPLDIR+RPLKGE                                            
*                                                                               
         GET   RPL=(5)             READ HI VSAM RECORD                          
         LTR   RF,RF                                                            
         JNZ   *+2                 VSAM ERROR BEFORE WAIT                       
         CHECK RPL=(5)                                                          
         OC    RPLFDBK,RPLFDBK                                                  
         JNZ   *+2                 VSAM ERROR AFTER WAIT                        
         L     R1,RPLRLEN          RETURNED RECORD LENGTH                       
         LA    R1,4(,R1)           ADD 4 FOR RDW LENGTH                         
         SLL   R1,16               AND SHIFT TO CORRECT POSITION                
         ST    R1,OTRDW                                                         
*                                                                               
         LLC   R1,KEYLEN           CHECK WE GOT THE KEY WE ASKED FOR            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         JE    MAIN30                                                           
         CLC   VSMKEY(0),OTVSREC                                                
*                                                                               
         AP    CTRNRF,=P'1'        COUNT RECORDS NOT FOUND                      
         LARL  R1,LTPNRF           RECORD NOT FOUND                             
         MVC   P(L'LTPNRF),0(R1)                                                
         MVC   P+L'LTPNRF(L'VSMKEY),VSMKEY                                      
         GOTOR VPRINTER                                                         
         LA    R1,16                                                            
         BRAS  RE,SETRC                                                         
         J     MAIN                                                             
*                                                                               
* COMPARE VSAM RECORD WITH INPUT                                                
*                                                                               
MAIN30   CLI   COMP,C'Y'           COMPARE=Y?                                   
         JNE   MAIN80                                                           
         LR    R0,R2               INPUT RECORD                                 
         LH    R1,0(,R2)           INPUT LENGTH                                 
         LR    RE,R3               VSAM RECORD                                  
         LH    RF,0(,R3)           VSAM LENGTH                                  
         CLCL  R0,RE                                                            
         JE    MAIN80              RECORDS MATCH                                
*                                                                               
         AP    CTRRNS,=P'1'        COUNT RECORDS NOT SAME                       
         LARL  R1,LTPRNS           RECORD NOT SAME                              
         MVC   P(L'LTPRNS),0(R1)                                                
         MVC   P+L'LTPNRF(L'VSMKEY),VSMKEY                                      
         GOTOR VPRINTER                                                         
         LA    R1,16                                                            
         BRAS  RE,SETRC                                                         
*                                                                               
* OUTPUT VSAM RECORD                                                            
*                                                                               
MAIN80   CLI   E35EXIT,C'Y'        TEST RUNNING AS A SORT E35 EXIT              
         JE    E35GOBAK            YES, JUST RETURN RECORD TO SORT              
*                                                                               
         LR    R0,R3                                                            
         LARL  R1,OFILE                                                         
         PUT   (1),(0)             WRITE RECORD                                 
*                                                                               
         J     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* END OF INPUT FILE                                                   *         
***********************************************************************         
EOFIN    CLOSE (IFILE,,DEMACB,,OFILE)                                           
         CLI   SPLIT,C'Y'          SPLIT FILE?                                  
         JNE   TOTALS              NO                                           
         CLOSE (DEMAC2)                                                         
         J     TOTALS                                                           
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT RECORD TOTALS ETC.                                            *         
***********************************************************************         
TOTALS   ZAP   LINE,=P'99'                                                      
         MVC   MID1,SPACES                                                      
         MVC   MID1(7),=C'SUMMARY'                                              
         MVC   MID2,SPACES                                                      
         MVC   MID2(7),=C'-------'                                              
*                                                                               
         LARL  R1,LTCIN                                                         
         MVC   P(L'LTCIN),0(R1)                                                 
         MVC   P+35(15),=X'4020206B2020206B2020206B202120'                      
         ED    P+35(15),CTRIN                                                   
         GOTOR VPRINTER                                                         
*                                                                               
         LARL  R1,LTCNRF                                                        
         MVC   P(L'LTCNRF),0(R1)                                                
         MVC   P+35(15),=X'4020206B2020206B2020206B202120'                      
         ED    P+35(15),CTRNRF                                                  
         GOTOR VPRINTER                                                         
*                                                                               
         LARL  R1,LTCRNS                                                        
         MVC   P(L'LTCRNS),0(R1)                                                
         MVC   P+35(15),=X'4020206B2020206B2020206B202120'                      
         ED    P+35(15),CTRRNS                                                  
         GOTOR VPRINTER                                                         
*                                                                               
         L     R2,VPRNTER          MUST CLOSE PRINT FILE BECAUSE SORT           
         CLOSE ((R2))              FREES ITS STORAGE IF RUNNING AND             
*                                  JOB ABENDS SC03                              
         CLI   E35EXIT,C'Y'        IF NOT RUNNING AS A SORT EXIT                
         JNE   PROGEXIT            USE NORMAL RETURN TO MVS.                    
*                                                                               
         MVC   E35RC,=H'8'         SET RC=8 - EOF                               
         J     E35GOBAK            RETURN TO SORT                               
*                                                                               
PROGEXIT XBASE RC=RETCODE,RL=2                                                  
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
SETRC    CH    R1,RETCODE          SET RETURN CODE IF HIGHER                    
         BNHR  RE                                                               
         STH   R1,RETCODE                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETER CARDS                                            *         
***********************************************************************         
VALPARMS NTR1                                                                   
         XC    RUNVALS(RUNVALL),RUNVALS                                         
         MVC   MID1,SPACES                                                      
         MVC   MID1(14),=C'CONTROL CARDS.'                                      
         MVC   MID2,SPACES                                                      
         MVC   MID2(14),=C'--------------'                                      
*                                                                               
VALPAR02 GOTOR VCARDS,DMCB,C,RE10 (1=TREAT OPEN ERROR AS EOF)                   
         MVC   P(L'C),C                                                         
         GOTOR VPRINTER                                                         
         CLI   C,C'*'              IGNORE COMMENTS                              
         JE    VALPAR02                                                         
         LA    R1,PARAMS           R1=A(PARAMETER TABLE)                        
         SR    RE,RE                                                            
VALPAR04 ICM   RE,1,0(R1)          TEST FOR END OF TABLE                        
         JNZ   *+12                                                             
         BRAS  RE,VALPARIV         YES - NOT A VALID CONTROL CARD               
         J     VALPAR02                                                         
         EX    RE,*+8              MATCH CARD DATA TO TABLE ENTRY               
         JE    VALPAR06                                                         
         CLC   C(0),4(R1)                                                       
         AHI   R1,L'PARAMS         BUMP TO NEXT TABLE ENTRY                     
         J     VALPAR04                                                         
*                                                                               
VALPAR06 SR    RF,RF               PROCESS PARAMETER CARD                       
         ICM   RF,7,1(R1)          RF=A(PROCESS/VALIDATION ROUTINE)             
         LA    R1,C+1(RE)          R1=A(DATA VALUE)                             
         GOTOR (RF),(R1)           CALL PROCESS/VALIDATE ROUTINE                
         J     VALPAR02                                                         
*                                                                               
VALPAR10 CLI   INPARM,0            INPUT= INPUT?                                
         JNE   VALPAR12            YES                                          
         MVI   INPARM,C'V'         DEFAULT INPUT=VSAM                           
         CLI   COMP,0              COMPARE= INPUT?                              
         JNE   VALPARX             YES                                          
         MVI   COMP,C'Y'           NO, DFLT COMPARE=Y WITH INPUT=VSAM           
         J     VALPARX                                                          
*                                                                               
VALPAR12 CLI   COMP,C'Y'           COMPARE=Y INPUT?                             
         JNE   VALPARX             NO                                           
         CLI   INPARM,C'V'         MUST HAVE INPUT=V                            
         JE    VALPARX                                                          
         LARL  R1,LTPCNV                                                        
         MVC   P+4(L'LTPCNV),0(R1)                                              
         J     VALPARER                                                         
*                                                                               
VALPARIV LARL  R1,LTPICC                                                        
         MVC   P+4(L'LTPICC),0(R1)                                              
         J     VALPARER                                                         
                                                                                
VALPARDP LARL  R1,LTPDPC                                                        
         MVC   P+4(L'LTPDPC),0(R1)                                              
                                                                                
VALPARER MVC   P(3),=C'***'                                                     
         LR    R0,RE                                                            
         GOTOR VPRINTER                                                         
         LR    RE,R0                                                            
         MVI   ABEND,C'Y'                                                       
         BR    RE                                                               
                                                                                
VALPARX  CLI   ABEND,C'Y'                                                       
         JE    VALPARAB                                                         
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         ZAP   LINE,=P'99'                                                      
         J     EXIT                                                             
VALPARAB GOTOR VPRINTER                                                         
         LARL  R1,LTPRAB                                                        
         MVC   P+4(L'LTPRAB),0(R1)                                              
         GOTOR (RF)                                                             
         ABEND 100                                                              
*                                                                               
PARAMS   DS    0XL24               ** TABLE OF PARAMETER CARDS **               
         DC    AL1(05),AL3(PARINPT),CL20'INPUT='                                
         DC    AL1(07),AL3(PARCOMP),CL20'COMPARE='                              
         DC    AL1(05),AL3(PARSPLT),CL20'SPLIT='                                
         DC    AL1(01),AL3(VALPAR10),CL20'/*'                                   
         DC    AL1(0)                                                           
*                                                                               
* PARAMETER VALIDATION ROUTINES R1=A(PARAMTER VALUE)                            
*                                                                               
PARINPT  CLI   INPARM,0            INPUT=                                       
         JNE   VALPARDP                                                         
         MVC   INPARM,0(R1)                                                     
         CLI   INPARM,C'D'         INPUT=D(ANDX)                                
         BER   RE                                                               
         CLI   INPARM,C'V'         INPUT=V(SAM)                                 
         BER   RE                                                               
         J     VALPARIV            (RETURNS TO RE)                              
*                                                                               
PARCOMP  CLI   COMP,0              COMPARE=                                     
         JNE   VALPARDP                                                         
         MVC   COMP,0(R1)                                                       
         CLI   COMP,C'Y'           COMPARE=Y(ES)                                
         BER   RE                                                               
         CLI   COMP,C'N'           COMPARE=N(O)                                 
         BER   RE                                                               
         J     VALPARIV            (RETURNS TO RE)                              
*                                                                               
PARSPLT  CLI   SPLIT,0             SPLIT=                                       
         JNE   VALPARDP                                                         
         MVC   SPLIT,0(R1)                                                      
         CLI   SPLIT,C'Y'          SPLIT=Y(ES)                                  
         BER   RE                                                               
         CLI   SPLIT,C'N'          SPLIT=N(O)                                   
         BER   RE                                                               
         J     VALPARIV            (RETURNS TO RE)                              
         EJECT                                                                  
***********************************************************************         
* STORAGE                                                             *         
***********************************************************************         
*              LITERALS                                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              OTHER W/S REQUIREMENTS                                           
*                                                                               
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    6F                                                               
SAVERE   DS    F                                                                
PL12     DS    PL12                                                             
PL6      DS    PL6                                                              
C        DS    CL80                                                             
*                                                                               
RETCODE  DC    H'0'                MVS RETURN CODE                              
*                                                                               
VCPRINT  DC    V(CPRINT)                                                        
VPRINTER DC    V(PRINTER)                                                       
VPRNTER  DC    V(PRNTER)           PRINT DCB WITHIN DDPRINT                     
VCARDS   DC    V(CARDS)                                                         
*                                                                               
KEYLEN   DS    C                                                                
*                                                                               
VSMKEY   DS    0XL21               VSAM KEY                                     
VSMKDND  DC    0XL20               DANDX KEY                                    
VSMKMAJ  DC    XL18'00'            MAJOR KEY                                    
VSMKMIN  DC    XL2'00'             MINOR KEY                                    
VSMKSEQ  DC    XL1'00'             SEQUENCE                                     
*                                                                               
RUNVALS  DS    0X                  ** RUN TIME PARAMETERS **                    
SPLIT    DS    C                   Y IF SPLIT=Y ELSE N OR NULL                  
COMP     DS    C                   Y IF COMPARE TO BE USED                      
INPARM   DS    C                   V IF INPUT=VSAM ELSE D FOR DANDX             
RUNVALL  EQU   *-RUNVALS                                                        
*                                                                               
ABEND    DC    C'N'                                                             
*                                                                               
E35EXIT  DC    C'N'                Y IF CALLED AS EXIT FROM SORT                
E35RC    DC    H'0'                RETURN CODE FOR SORT IF EXIT                 
*                                                                               
         DS    0D                                                               
CTRS     DS    0PL6                                                             
CTRIN    DC    PL6'0'              ALL RECORDS READ                             
CTRNRF   DC    PL6'0'              KEY NOT ON VSAM FILE                         
CTRRNS   DC    PL6'0'              RECORD ON VSAM FILE NOT SAME                 
*                                                                               
RE10     DC    C'RE10'                                                          
*                                                                               
         DS    0A                                                               
DSNXTRCT DC    X'87',AL3(0)                                                     
*                                                                               
*              DCBS, ETC.                                                       
*                                                                               
IFILE    DCB   DDNAME=IFILE,DSORG=PS,MACRF=(GL),RECFM=VB,EODAD=EOFIN            
*                                                                               
OFILE    DCB   DDNAME=OFILE,DSORG=PS,MACRF=(PM),RECFM=VB,LRECL=8192             
*                                                                               
DEMACB   ACB   AM=VSAM,DDNAME=DEMVSM,MACRF=(KEY,DIR,SKP,IN),           X        
               BUFNI=10,BUFND=16,RMODE31=ALL                                    
*                                                                               
DEMRPL   RPL   ACB=DEMACB,AM=VSAM,                                     X        
               AREA=VSAREA,AREALEN=(L'VSAREA),ARG=VSMKEY,              X        
               OPTCD=(KEY,FWD,SYN,NSP,KGE)                                      
*                                                                               
DEMAC2   ACB   AM=VSAM,DDNAME=DEMVS2,MACRF=(KEY,DIR,SKP,IN),           X        
               BUFNI=10,BUFND=16,RMODE31=ALL                                    
*                                                                               
DEMRP2   RPL   ACB=DEMAC2,AM=VSAM,                                     X        
               AREA=VSAREA,AREALEN=(L'VSAREA),ARG=VSMKEY,              X        
               OPTCD=(KEY,FWD,SYN,NSP,KGE)                                      
*                                                                               
*              I/O AREAS                                                        
*                                                                               
         DS    0L                                                               
OUTAREA  DS    CL(8*1024)                                                       
VSAREA   EQU   OUTAREA+4           VSAM INPUT AREA (SEE DEMRPL)                 
*                                                                               
*              LITERALS USING LARL (ALL MUST BE HW ALIGNED)                     
*                                                                               
         DS    0H                                                               
LTCIN    DC    C'RECORDS READ',0H'0'                                            
LTCNRF   DC    C'KEY NOT FOUND',0H'0'                                           
LTCRNS   DC    C'KEY FOUND, DATA DIFFERS',0H'0'                                 
*                                                                               
LTPCNV   DC    C'COMPARE=Y VALID ONLY WITH INPUT=VSAM',0H'0'                    
LTPICC   DC    C'INVALID CONTROL CARD',0H'0'                                    
LTPDPC   DC    C'DUPLICATE CARD',0H'0'                                          
LTPRAB   DC    C'RUN ABORTED DUE TO ABOVE ERRORS',0H'0'                         
LTPNRF   DC    C'RECORD NOT FOUND. KEY=',0H'0'                                  
LTPRNS   DC    C'RECORDS NOT SAME. KEY=',0H'0'                                  
         EJECT                                                                  
***********************************************************************         
* ENTRY FROM AND RETURN TO SORT WHEN RUNNING AS A SORT E35 EXIT       *         
* FIRST TEST IF WE ARE ENTERED FROM SORT. IF NOT, NORMAL CODE         *         
***********************************************************************         
         USING E35,RF                                                           
E35      SAVE  (14,12),,DEMVSCAN   ENTRY GOOD FOR MVS AS WELL AS SORT           
         CLC   1(3,RD),=C'SM1'     SORT ALWAYS CALLS WITH THIS                  
         JNE   DEMVSCAN            USUAL NBASE IF NOT FROM SORT                 
         STMH  GR0,GRF,DFSORTHH                                                 
         LRL   RE,=V(REGSAVE)      GET OUR SAVE AREA CHAIN                      
         ST    RD,4(,RE)           SAVE FORWARD POINTER IN OURS                 
         ST    RE,8(,RD)           ...IN SAVE AREA                              
         LR    RD,RE               SET OUR SAVE AREA                            
         L     R2,0(,R1)           GET ADDRESS OF RECORD                        
         LARL  RB,DEMVSCAN                                                      
         LAY   RA,4096(,RB)                                                     
         MVI   E35EXIT,C'Y'                                                     
         XC    E35RC,E35RC         PRESET RC=0                                  
         LARL  R3,OUTAREA                                                       
         L     R9,VCPRINT                                                       
         BRC   0,MAIN              GO BACK TO MAIN IF NOT FIRST TIME            
         OI    *-3,X'F0'           *** ONLY DO THIS ONCE   ***                  
         J     INIT                GO TO INIT FIRST TIME IN                     
*                                                                               
E35GOBAK LARL  RF,E35              RESET E35 BASE                               
         SGR   GR1,GR1                                                          
         LR    R1,R3               SET RECORD POINTER                           
         LMH   GR0,GR0,DFSORTHH    RESTORE HIGH HALVES OF SORT'S REGS           
         LMH   GR2,GRE,DFSORTHH+8                                               
         LGHRL GRF,E35RC           SET RC (WAS OUR BASE)                        
         L     RD,4(,RD)                                                        
         L     RE,12(,RD)                                                       
         LM    R2,RC,28(RD)        RESTORE SORT'S REGS                          
         BSM   0,RE                RETURN TO SORT                               
*                                                                               
DFSORTHH DS    8D                  HIGH HALVES OF SORT'S REGISTERS              
         LTORG                                                                  
         DROP  RF                                                               
         EJECT                                                                  
*                                                                               
*              RECORD DSECTS                                                    
*                                                                               
INREC    DSECT ,                   RECORD FROM DELDXMOD OUTPUT                  
INRDW    DS    0XL4                RDW                                          
INLENMVS DS    XL2                 MVS RECORD LENGTH                            
         DS    XL2                                                              
INKEY    DS    0C                                                               
INPLENQ  EQU   4+18+4+1            PASSIVE LNGTH (RDW,KEY,DATA,STA)             
*                                                                               
OTREC    DSECT ,                   OUTPUT RECORD                                
OTLENMVS DS    0XL2                MVS RECORD LENGTH                            
OTRDW    DS    XL4                 MVS RDW                                      
OTVSREC  DS    0C                  VSAM RECORD                                  
*                                                                               
       ++INCLUDE DEVSMFILE                                                      
         EJECT                                                                  
         IFGACB AM=VSAM                                                         
         IFGRPL AM=VSAM                                                         
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DEMVSCAN  03/30/17'                                      
         END                                                                    

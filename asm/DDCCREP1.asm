*          DATA SET DDCCREP1   AT LEVEL 218 AS OF 01/19/01                      
*PHASE CCREP1,*                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE CARDS                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
CCREP1   CSECT                                                                  
         NBASE WORKX-WORKD,XXRMORXX,R9,WORK=A(WORKAREA),CLEAR=YES               
         USING WORKD,RC                                                         
         USING PLINED,PLINE                                                     
         SPACE 1                                                                
*                                                                               
         L     R1,=A(CSECTS-WORKD) GET OUT OF RANGE WORK                        
         AR    R1,RC                                                            
         ST    R1,ACSECTS                                                       
         L     R1,=A(PSAVED-WORKD) GET OUT OF RANGE WORK                        
         AR    R1,RC                                                            
         ST    R1,APSAVED                                                       
         ST    R1,APSAVNDX                                                      
*                                  INITIALISE STORAGE                           
INIT01   OPEN  (PANFILE,INPUT)                                                  
         BAS   RE,PRINTI                                                        
         BAS   RE,INIT                                                          
         CLI   LOGOPT,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,LOGINI                                                        
         BAS   RE,MAIN                                                          
         BAS   RE,PRINTX                                                        
         CLOSE PANFILE                                                          
*                                                                               
XBASE    XBASE                                                                  
*                                                                               
PANXX    TM    FLAG1,X'40'         TEST WHICH EXIT TO TAKE                      
         BO    PANXX2                                                           
         B     PANXX1                                                           
         EJECT                                                                  
*************************************************************                   
*        INIT LOG SCAN                                      *                   
*************************************************************                   
         SPACE 1                                                                
LOGINI   NTR1                                                                   
         OPEN  (LOGIN,INPUT)       OPEN LOG FILE                                
         LA    R2,IOAREA                                                        
         LR    R1,R2                                                            
         AH    R1,=H'4096'         SAVE 4K LIMIT IN FULL                        
         ST    R1,FULL                                                          
         XC    0(10,R2),0(R2)      CLEAR FIRST ENTRY                            
*                                                                               
LOGIN010 LA    R2,IOAREA                                                        
         GET   LOGIN                                                            
LOGIN005 OC    0(10,R2),0(R2)      TEST ENTRY                                   
         BZ    LOGIN020                                                         
*                                                                               
         CLC   0(10,R2),0(R1)      DO WE HAVE IT ALREADY                        
         BE    LOGIN010                                                         
         LA    R2,10(R2)           NEXT ENTRY                                   
         B     LOGIN005                                                         
*                                                                               
LOGIN020 MVC   0(10,R2),0(R1)      SAVE PANBOOK NAME                            
         LA    R2,10(R2)                                                        
         XC    0(10,R2),0(R2)                                                   
         C     R2,FULL                                                          
         BL    LOGIN010                                                         
         DC    H'0'                TOO MANY ENTRIES                             
*                                                                               
LOGXX    CLOSE LOGIN                                                            
         L     R4,ACSECTS          R4=CSECT POINTER                             
         LR    R1,R4                                                            
         AH    R1,=H'4096'         SAVE 4K LIMIT IN FULL                        
         ST    R1,FULL                                                          
         OI    FLAG1,X'40'         FLAG PANXX2                                  
*                                                                               
LOGIN100 LA    R3,IOAREA           FIRST PASS PANFILE                           
         GET   PANFILE                                                          
         LR    R2,R1                                                            
         USING PANRECD,R2                                                       
LOGIN110 OC    0(10,R3),0(R3)      IF EOT GET NEXT BOOK                         
         BZ    LOGIN100                                                         
*                                                                               
         CLC   PRBOOK,0(R3)        COMPARE BOOKNAME                             
         BE    LOGIN150                                                         
         LA    R3,10(R3)           NEXT ENTRY                                   
         B     LOGIN110                                                         
*                                                                               
LOGIN150 L     R4,ACSECTS                                                       
LOGIN151 OC    0(8,R4),0(R4)       TEST FOR EOT                                 
         BE    LOGIN155                                                         
         CLC   0(8,R4),PRCSECT     HAVE WE GOT IT ALREADY                       
         BE    LOGIN100                                                         
         LA    R4,8(R4)            TRY NEXT                                     
         B     LOGIN151                                                         
*                                                                               
LOGIN155 MVC   0(8,R4),PRCSECT     SAVE CSECT NAME                              
         LA    R4,8(R4)                                                         
         XC    0(8,R4),0(R4)                                                    
         C     R4,FULL                                                          
         BL    LOGIN100                                                         
         DC    H'0'                TOO MANY ENTRIES                             
*                                                                               
PANXX2   L     R4,ACSECTS          R4=CSECT POINTER                             
         CLOSE PANFILE                                                          
         OPEN  (PANFILE,INPUT)     RESET FOR REPORT                             
*                                                                               
LOGINX   XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        INITIALISE                                         *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
         LA    R3,IOAREA                                                        
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    EXIT                                                             
         MVC   PLINE+1(80),0(R3)                                                
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BNE   XBASE               NEQ MEANS INVALID KEYWORD                    
         B     INIT010                                                          
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        OPEN OUTPUT DATA SET                               *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
         BAS   RE,PRINTT           PRINT TITLE                                  
         MVI   SAVELINE,0                                                       
         MVC   PLINE,BK                                                         
         OI    FLAG1,X'80'         FLAG FIRST TIME                              
*                                                                               
MAIN010  GET   PANFILE             GET A LINE                                   
         LR    R2,R1                                                            
         USING PANRECD,R2                                                       
*                                                                               
         BAS   RE,FILTER           FILTER UNWANTED RECORDS                      
         BNE   MAIN010                                                          
*                                                                               
         CLC   SVCSECT,PRCSECT     PRINT A BREAK ON CSECT CHANGE                
         BE    MAIN011                                                          
         TM    FLAG1,X'80'                                                      
         BO    MAIN011                                                          
*                                                                               
         CLI   PFILTER,C'Y'                                                     
         BE    *+12                                                             
         BAS   RE,PRINTM                                                        
         B     *+12                                                             
         BAS   RE,PRINTFLT                                                      
         BAS   RE,PRINTMF                                                       
*                                                                               
MAIN011  NI    FLAG1,255-X'80'     TURN OFF FIRST TIME FLAG                     
         MVC   SVCSECT,PRCSECT                                                  
*                                                                               
         MVC   DUB,PRDATE          CONVERT DATE TO RECDATE                      
         MVC   RECDATE,=C'.......   '                                           
*                                                                               
         MVC   DUB1(6),DUB         USE THIS TO PROTECT DATCON                   
         NC    DUB1(6),=X'F0F0F0F0F0F0'                                         
         CLC   DUB1(6),=X'F0F0F0F0F0F0'                                         
         BNE   MAIN030                                                          
*                                                                               
         GOTO1 =V(DATCON),DMCB,(0,DUB),(8,RECDATE)                              
*                                                                               
MAIN030  MVC   PLINE,BK                                                         
*                                                                               
         MVC   PBDATE(7),RECDATE                                                
         MVC   PBLEVEL(3),PRLEVEL                                               
*                                                                               
         CLI   PRSRC,C'A'                                                       
         BNE   *+10                                                             
         MVC   PBSRC(3),=C'YES'                                                 
         CLI   PRRMB,C'B'                                                       
         BNE   *+10                                                             
         MVC   PBRMB(3),=C'YES'                                                 
         CLI   PRINC,C'C'                                                       
         BNE   *+10                                                             
         MVC   PBINC(3),=C'YES'                                                 
         CLI   PRPHS,C'D'                                                       
         BNE   *+10                                                             
         MVC   PBPHS(3),=C'YES'                                                 
*                                                                               
         MVC   PBCTR(2),PRCTR                                                   
         MVC   PCSECT(8),PRCSECT                                                
         MVC   PBOOK(10),PRBOOK                                                 
         MVC   PBRMBK(8),PRRMBK                                                 
         MVC   PINCLUD(10),PRINBK                                               
         MVC   PPHASE(8),PRPHASE                                                
*                                                                               
MAIN050  CLI   PFILTER,C'Y'                                                     
         BE    *+12                                                             
         BAS   RE,PRINTL                                                        
         B     *+8                                                              
         BAS   RE,PRINTLF                                                       
*                                                                               
MAIN060  B     MAIN010                                                          
*                                                                               
PANXX1   MVC   PLINE,B1                                                         
         BAS   RE,PRINTL                                                        
*                                                                               
XIT1     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        FILTER                                             *                   
*************************************************************                   
         SPACE 1                                                                
FILTER   NTR1                                                                   
*                                                                               
         CLI   LOGOPT,C'Y'         FOR LOG OPTION USE CSECT FILTER              
         BE    CSFILT                                                           
*                                                                               
         CLI   PHASE,C'*'          TEST FOR PHASE FILTER                        
*        BE    FILT010                                                          
         MVI   FILTERC,C'P'                                                     
         XC    FILTER1,FILTER1                                                  
         XC    FILTER2,FILTER2                                                  
         MVC   FILTER1(8),PHASE                                                 
         MVC   FILTER2(8),PRPHASE                                               
         BAS   RE,FILT100                                                       
         BE    FILT010             OK PHASE MATCHES                             
*                                                                               
         XC    FILTER1,FILTER1                                                  
         XC    FILTER2,FILTER2                                                  
         MVC   FILTER1(8),CHECK    DOES PHASE MATCH CHECK                       
         MVC   FILTER2(8),PRPHASE                                               
         BAS   RE,FILT100                                                       
         BE    FILT010             OK PHASE MATCHES                             
         B     FILTERN                                                          
*                                                                               
FILT010  CLI   CSECT,C'*'          TEST FOR CSECT FILTER                        
*        BE    FILT020                                                          
         MVI   FILTERC,C'C'                                                     
         XC    FILTER1,FILTER1                                                  
         XC    FILTER2,FILTER2                                                  
         MVC   FILTER1(8),CSECT                                                 
         MVC   FILTER2(8),PRCSECT                                               
         BAS   RE,FILT100                                                       
         BNE   FILTERN                                                          
*                                                                               
FILT020  CLI   PANBOO,C'*'         TEST FOR PANBOOK FILTER                      
*        BE    FILT030                                                          
         MVI   FILTERC,C'B'                                                     
         XC    FILTER1,FILTER1                                                  
         XC    FILTER2,FILTER2                                                  
         MVC   FILTER1(10),PANBOO                                               
         MVC   FILTER2(10),PRBOOK                                               
         BAS   RE,FILT100                                                       
         BNE   FILTERN                                                          
*                                                                               
FILT030  CLI   RMBOOK,C'*'         TEST FOR RMBOOK FILTER                       
*        BE    FILT040                                                          
         MVI   FILTERC,C'R'                                                     
         XC    FILTER1,FILTER1                                                  
         XC    FILTER2,FILTER2                                                  
         MVC   FILTER1(8),RMBOOK                                                
         MVC   FILTER2(8),PRRMBK                                                
         BAS   RE,FILT100                                                       
         BNE   FILTERN                                                          
*                                                                               
FILT040  CLI   INCLUD,C'*'         TEST FOR INCLUDE FILTER                      
*        BE    FILT050                                                          
         MVI   FILTERC,C'I'                                                     
         XC    FILTER1,FILTER1                                                  
         XC    FILTER2,FILTER2                                                  
         MVC   FILTER1(8),INCLUD                                                
         MVC   FILTER2(8),PRINBK                                                
         BAS   RE,FILT100                                                       
         BNE   FILTERN                                                          
*                                                                               
FILT050  CLI   CTRY,C'*'           TEST FOR CTRY FILTER                         
         BE    FILTERY                                                          
         CLC   CTRY,PRCTR                                                       
         BNE   FILTERN                                                          
         B     FILTERY                                                          
*                                                                               
FILT100  NTR1                                                                   
         LA    R0,12               UP TO 12 CHRS                                
         LA    R1,FILTER1                                                       
         LA    RF,FILTER2                                                       
FILT110  CLI   0(R1),C'*'          WILDCARDS ARE OK                             
         BE    FILT120                                                          
         CLI   0(R1),C' '          BLANKS OR LESS ARE OK                        
         BNH   FILT120                                                          
         CLC   0(1,R1),0(RF)       A PERFECT MATCH IS OK TOO                    
         BE    FILT120                                                          
*                                                                               
         B     FILTERN             ELSE NO MATCH                                
*                                                                               
FILT120  LA    RF,1(RF)            TRY NEXT                                     
         LA    R1,1(R1)                                                         
         BCT   R0,FILT110                                                       
*                                                                               
         CLI   XBOOK,C'Y'          EX BOOKS AS WELL                             
         BE    FILTERY             CONGRATULATIONS                              
*                                                                               
         LA    RF,FILTER2                                                       
         LA    R0,12               UP TO 11 CHRS                                
FILT200  CLC   0(2,RF),=C'XX'                                                   
         BE    FILTERN                                                          
         CLC   0(2,RF),=C'X '                                                   
         BE    FILTERN                                                          
         CLC   0(2,RF),=X'E700'                                                 
         BE    FILTERN                                                          
*                                                                               
         CLI   FILTERC,C'P'        FOR PHASES ONLY                              
         BNE   FILTERY                                                          
*                                                                               
         CLC   0(2,RF),=C'S '      CHECK FOR S                                  
         BE    FILTERN                                                          
         CLC   0(2,RF),=X'E200'                                                 
         BE    FILTERN                                                          
*                                                                               
FILT210  LA    RF,1(RF)            TRY NEXT                                     
         BCT   R0,FILT200                                                       
         B     FILTERY             CONGRATULATIONS                              
*                                                                               
CSFILT   L     R4,ACSECTS                                                       
CSF010   CLI   0(R4),0             NOT FOUND IF EOT                             
         BE    FILTERN                                                          
         CLC   0(8,R4),PRCSECT     YES FOUND                                    
         BE    FILTERY                                                          
         LA    R4,8(R4)                                                         
         B     CSF010                                                           
*                                                                               
FILTERN  LTR   RB,RB                                                            
         B     XIT1                                                             
FILTERY  CR    RB,RB                                                            
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        REPORT LINES                                       *                   
*************************************************************                   
         SPACE 1                                                                
T1       DS    CL166                                                            
         ORG   T1                                                               
         DC    C'1'                                                             
         DC    AL1(TL),15AL1(HB)                                                
         DC    AL1(TM),10AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),05AL1(HB)                                                
         DC    AL1(TM),15AL1(HB)                                                
         DC    AL1(TM),15AL1(HB)                                                
         DC    AL1(TM),15AL1(HB)                                                
         DC    AL1(TM),15AL1(HB)                                                
         DC    AL1(TR)                                                          
         ORG                                                                    
*                                                                               
T2       DS    CL166                                                            
         ORG   T2                                                               
         DC    C' '                                                             
         DC    AL1(VB),CL15'SOURCE BOOK'                                        
         DC    AL1(VB),CL10'DATE'                                               
         DC    AL1(VB),CL05'LEVEL'                                              
         DC    AL1(VB),CL05'PAN  '                                              
         DC    AL1(VB),CL05'RM   '                                              
         DC    AL1(VB),CL05'*INC '                                              
         DC    AL1(VB),CL05'PHASE'                                              
         DC    AL1(VB),CL05'CTRY '                                              
         DC    AL1(VB),CL15'CSECT'                                              
         DC    AL1(VB),CL15'RMBOOK'                                             
         DC    AL1(VB),CL15'*INCLUDE'                                           
         DC    AL1(VB),CL15'PHASE'                                              
         DC    AL1(VB)                                                          
         ORG                                                                    
*                                                                               
BK       DS    CL166                                                            
         ORG   BK                                                               
         DC    C' '                                                             
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB),10AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),05AL1(BB)                                                
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB),15AL1(BB)                                                
         DC    AL1(VB)                                                          
         ORG                                                                    
*                                                                               
M1       DS    CL166                                                            
         ORG   M1                                                               
         DC    C' '                                                             
         DC    AL1(ML),15AL1(HB)                                                
         DC    AL1(MM),10AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),05AL1(HB)                                                
         DC    AL1(MM),15AL1(HB)                                                
         DC    AL1(MM),15AL1(HB)                                                
         DC    AL1(MM),15AL1(HB)                                                
         DC    AL1(MM),15AL1(HB)                                                
         DC    AL1(MR)                                                          
         ORG                                                                    
*                                                                               
B1       DS    CL166                                                            
         ORG   B1                                                               
         DC    C' '                                                             
         DC    AL1(BL),15AL1(HB)                                                
         DC    AL1(BM),10AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),05AL1(HB)                                                
         DC    AL1(BM),15AL1(HB)                                                
         DC    AL1(BM),15AL1(HB)                                                
         DC    AL1(BM),15AL1(HB)                                                
         DC    AL1(BM),15AL1(HB)                                                
         DC    AL1(BR)                                                          
         ORG                                                                    
         EJECT                                                                  
*************************************************************                   
*        BOX EQUATES                                        *                   
*************************************************************                   
         SPACE 1                                                                
TL       EQU   X'AC'               TOP LEFT                                     
TM       EQU   X'CC'               TOP MIDDLE                                   
TR       EQU   X'BC'               TOP RIGHT                                    
HB       EQU   X'BF'               HORIZONTAL BAR                               
VB       EQU   X'FA'               VERTICAL  BAR                                
ML       EQU   X'EB'               MIDDLE LEFT                                  
MM       EQU   X'8F'               MIDDLE MIDDLE                                
MR       EQU   X'EC'               MIDDLE RIGHT                                 
BL       EQU   X'AB'               BOTTOM LEFT                                  
BM       EQU   X'CB'               BOTTOM MIDDLE                                
BR       EQU   X'BB'               BOTTOM RIGHT                                 
BB       EQU   X'40'               BLANK LINE                                   
         EJECT                                                                  
*************************************************************                   
*        PRINT FILTERS                                      *                   
*************************************************************                   
         SPACE 1                                                                
PRINTFLT NTR1                                                                   
*                                                                               
         L     R2,APSAVED          POINT TO FIRST PLINE                         
         LA    R2,166(R2)          SKIP MID LINE                                
         MVC   CSSAVE(31),64(R2)   COPY CSECT DETAIL                            
         MVC   LVSAVE(32),0(R2)    COPY LEVEL DETAIL                            
*                                                                               
         L     R2,APSAVED          POINT TO FIRST PLINE                         
         LA    R2,166(R2)          SKIP MID LINE                                
*                                                                               
PRF010   MVC   PLINE,0(R2)                                                      
         MVC   WORK+0(3),PBSRC     COPY YESYESYESYES                            
         MVC   WORK+3(3),PBRMB                                                  
         MVC   WORK+6(3),PBINC                                                  
         MVC   WORK+9(3),PBPHS                                                  
*                                                                               
         CLC   WORK(12),=C'YESYESYESYES'                                        
         BE    PRF015                                                           
         CLC   WORK(12),=C'YES      YES'                                        
         BE    PRF015                                                           
*                                                                               
         CLI   NOLINK,C'N'         SKIP NOLINK                                  
         BNE   PRF900                                                           
*                                                                               
         CLC   WORK(12),=C'YESYES   YES'                                        
         BE    PRF015                                                           
         CLC   WORK(12),=C'      YES   '                                        
         BE    PRF020                                                           
         B     PRF900                                                           
*                                                                               
PRF015   CLC   CSSAVE(31),64(R2)   CHECK CSECT DETAIL                           
         BNE   PRF900                                                           
         CLC   LVSAVE(32),0(R2)    CHECK LEVEL DETAIL                           
         BNE   PRF900                                                           
*                                                                               
*                                                                               
PRF020   LA    R2,166(R2)          NEXT                                         
         C     R2,APSAVNDX                                                      
         BNL   PRINTFX             IF ALL SAME AND COMPLETE IGNORE IT           
         B     PRF010                                                           
*                                                                               
PRF900   L     R2,APSAVED          POINT TO FIRST PLINE                         
PRF910   MVC   PLINE,0(R2)                                                      
         BAS   RE,PRINTL           PRINT THEM OUT                               
         LA    R2,166(R2)                                                       
         C     R2,APSAVNDX                                                      
         BL    PRF910                                                           
PRINTFX  B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
*        LOAD PRINTER BUFFER                               *                    
************************************************************                    
*                                                                               
PRINTMF  L     R1,APSAVED                                                       
         MVC   0(166,R1),M1                                                     
         LA    R1,166(R1)                                                       
         XC    0(166,R1),0(R1)                                                  
         ST    R1,APSAVNDX                                                      
         BR    RE                                                               
*                                                                               
PRINTLF  L     R1,APSAVNDX                                                      
         MVC   0(166,R1),PLINE                                                  
         LA    R1,166(R1)                                                       
         XC    0(166,R1),0(R1)                                                  
         ST    R1,APSAVNDX                                                      
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
         SPACE 1                                                                
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
PRINTT1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
*                                                                               
         PUT   SYSPRINT,T1         PRINT TITLES                                 
         PUT   SYSPRINT,T2                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTM   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BNL   PRINTT1                                                          
*                                                                               
         PUT   SYSPRINT,M1         PRINT MIDLINE                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
         ZAP   LINE,=P'4'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
*                                                                               
         PUT   SYSPRINT,T1         PRINT TITLES                                 
         PUT   SYSPRINT,T2                                                      
         PUT   SYSPRINT,M1                                                      
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
*        MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
************************************************************                    
*        VALIDATE PARAMETER CARDS                          *                    
************************************************************                    
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN-1),X'FLAGS',AL3(OUTPUT)           
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*                                                                               
         SPACE 1                                                                
CARDTAB  DS    0F                                                               
         DC    C'PHASE  ',AL1(4,8),X'0000',AL3(PHASE)                           
         DC    C'CHECK  ',AL1(4,8),X'0000',AL3(CHECK)                           
         DC    C'CSECT  ',AL1(4,8),X'0000',AL3(CSECT)                           
         DC    C'PANBOOK',AL1(6,9),X'0000',AL3(PANBOO)                          
         DC    C'RMBOOK ',AL1(5,7),X'0000',AL3(RMBOOK)                          
         DC    C'INCLUDE',AL1(6,9),X'0000',AL3(INCLUD)                          
         DC    C'CTRY   ',AL1(3,2),X'0000',AL3(CTRY)                            
         DC    C'EX     ',AL1(1,1),X'0000',AL3(XBOOK)                           
         DC    C'NOLINK ',AL1(5,1),X'0000',AL3(NOLINK)                          
         DC    C'LOG    ',AL1(2,1),X'0000',AL3(LOGOPT)                          
         DC    C'FILTER ',AL1(5,1),X'0000',AL3(PFILTER)                         
         DC    X'0000'                                                          
*                                                                               
PHASE    DC    C'********'                                                      
CHECK    DC    C'.       '                                                      
PANBOO   DC    C'**********'                                                    
RMBOOK   DC    C'********'                                                      
CSECT    DC    C'********'                                                      
INCLUD   DC    C'**********'                                                    
CTRY     DC    C'**'                                                            
LOGOPT   DC    C'N'                                                             
XBOOK    DC    C'Y'                                                             
NOLINK   DC    C'Y'                                                             
PFILTER  DC    C'Y'                                                             
*                                                                               
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITEQU                                                          
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC090                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC090                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         DC    H'0'                NO-OP                                        
*        BAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC090                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         DC    H'0'                NO-OP                                        
*        BAS   RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC090                                                          
*                                                                               
VALC080  CLI   8(R4),0             DONT CARE                                    
         BE    *+12                                                             
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC090  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC090                                                          
*                                                                               
EXITEQU  CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
CERRSYS  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
CERRPRG  LA    R1,=C'INVALID PROGRAM '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SESYS   '                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
EXITNEQ  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*   DATA CONTROL BLOCKS                                     *                   
*************************************************************                   
         SPACE 1                                                                
PANFILE  DCB   DDNAME=PANFILE,DSORG=PS,MACRF=GL,EODAD=PANXX                     
LOGIN    DCB   DDNAME=LOGIN,DSORG=PS,MACRF=GL,RECFM=FB,EODAD=LOGXX              
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
*        NOTE  RECORD LEN=166 WITH CHARS=(BX15)                                 
*                                                                               
*************************************************************                   
*        LITERALS                                           *                   
*************************************************************                   
         SPACE 1                                                                
SPACES   DC    166C' '                                                          
MAXLINE  DC    PL4'60'                                                          
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        WORK AREA                                          *                   
*************************************************************                   
         SPACE 1                                                                
WORKAREA DC  30000D'0'                                                          
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
ACSECTS  DS    A                                                                
APSAVED  DS    A                                                                
APSAVNDX DS    A                                                                
*                                                                               
PLINE    DS    CL166                                                            
SAVELINE DS    CL166                                                            
*                                                                               
WORK     DS    CL166                                                            
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
SAVERE   DS    F                                                                
CARDEND  DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAG1    DS    X                                                                
DMCB     DS    6F                                                               
*                                                                               
RECDATE  DS    CL7                                                              
RECTYPE  DS    CL1                                                              
LINE     DS    PL4                                                              
PAGE     DS    PL4                                                              
*                                                                               
SVCSECT  DS    CL8                                                              
*                                                                               
FILTER1  DS    CL12                                                             
FILTER2  DS    CL12                                                             
*                                                                               
FILTERC  DS    C                   CURRENT FILTER VALUE                         
*                                                                               
CSSAVE   DS    CL31                                                             
LVSAVE   DS    CL32                                                             
*                                                                               
IOAREA   DS    4096C                                                            
CSECTS   DS    4096C                                                            
PSAVED   DS    1660000C             1000 SAVED PRINT LINES                      
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 1                                                                
PANRECD  DSECT                                                                  
PRCSECT  DS    CL8                                                              
         DS    CL1                                                              
PRDATE   DS    CL6                                                              
         DS    CL3                                                              
PRLEVEL  DS    CL3                                                              
         DS    CL1                                                              
PRSRC    DS    CL1                                                              
PRRMB    DS    CL1                                                              
PRINC    DS    CL1                                                              
PRPHS    DS    CL1                                                              
PRCTR    DS    CL2                                                              
         DS    CL1                                                              
PRBOOK   DS    CL10                                                             
         DS    CL1                                                              
PRRMBK   DS    CL8                                                              
         DS    CL1                                                              
PRINBK   DS    CL10                                                             
         DS    CL1                                                              
PRPHASE  DS    CL10                                                             
         DS    CL1                                                              
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL1                                                              
         DS    CL1                                                              
PBOOK    DS    CL15                                                             
         DS    CL1                                                              
PBDATE   DS    CL10                                                             
         DS    CL1                                                              
PBLEVEL  DS    CL05                                                             
         DS    CL1                                                              
PBSRC    DS    CL05                                                             
         DS    CL1                                                              
PBRMB    DS    CL05                                                             
         DS    CL1                                                              
PBINC    DS    CL05                                                             
         DS    CL1                                                              
PBPHS    DS    CL05                                                             
         DS    CL1                                                              
PBCTR    DS    CL05                                                             
         DS    CL1                                                              
PCSECT   DS    CL15                                                             
         DS    CL1                                                              
PBRMBK   DS    CL15                                                             
         DS    CL1                                                              
PINCLUD  DS    CL15                                                             
         DS    CL1                                                              
PPHASE   DS    CL15                                                             
         DS    CL1                                                              
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'218DDCCREP1  01/19/01'                                      
         END                                                                    

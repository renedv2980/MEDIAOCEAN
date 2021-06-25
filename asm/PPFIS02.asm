*          DATA SET PPFIS02    AT LEVEL 062 AS OF 03/28/07                      
*PHASE T41B02A                                                                  
*INCLUDE PUBEDIT                                                                
         TITLE 'PPFIS02 - PRINTPAK FIS  PUB DISPLAY'                            
*                                                                               
*     CHANGE LOG                                                                
*                                                                               
*   BOBY 01/05     2 CH MEDIA OFFICE CODES                                      
*                                                                               
*   SMYE 5/02      NEW LIMIT ACCESS SECURITY INCLUDING TRAFFIC OFFICE           
*                                                                               
*   BPLA 5/95      CLEAR DATASW AND MOVE PREVDSW TO IT                          
*                  ALSO SET IT TO X'01' IF I DISPLAY DATA THIS TIME             
*                                                                               
*   BPLA 3/17/94   IF DOING ONE PRD WITH CLT=*,&, OR $                          
*                  SKIP TO NEXT CLT - DON'T STOP LOOKING                        
*                                                                               
*   BPLA 3/1/94    CHANGES FOR BILLING GROUP (SVCLT=&)                          
*                                                                               
*   BPLA 11/5/93   CHANGES FOR OFFICE LIST (SVCLT=$)                            
*                                                                               
T41B02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41B02,RR=R9                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         USING T41BFFD,RA                                                       
         LA    RE,REC                                                           
         LA    RF,2000                                                          
         XCEF                                                                   
HEAD     XC    FISHD1,FISHD1                                                    
         FOUT  FISHD1H             CLEAR HEADS                                  
         XC    FISHD2,FISHD2                                                    
         CLI   PNAMESW,C'Y'        SEE IF DOING PUB NAME                        
         BNE   HEAD2                                                            
         MVC   FISHD2(11),=C'PUBLICATION'                                       
         MVC   FISHD2+20(4),=C'NAME'                                            
         CLC   SVPRD,=C'ALL'                                                    
         BNE   HEAD15                                                           
         MVC   FISHD2(15),=C'PRD/PUBLICATION'                                   
         MVC   FISHD2+20(8),=C'    NAME'                                        
         CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BE    HEAD1                                                            
         CLI   SVCLT,C'*'          SEE IF DOING AN OFFICE                       
         BE    HEAD1                                                            
         CLI   SVCLT,C'$'          SEE IF DOING AN OFFICE LIST                  
         BE    HEAD1                                                            
         CLC   SVCLT,=C'ALL'                                                    
         BNE   HEAD15                                                           
HEAD1    MVC   FISHD2(19),=C'CLT/PRD/PUBLICATION'                               
         MVC   FISHD2+24(8),=C'    NAME'                                        
         B     HEAD15                                                           
*                                                                               
HEAD2    DS    0H                                                               
         CLC   SVPRD,=C'ALL'                                                    
         BE    HEAD5                                                            
         MVC   FISHD2(11),=C'PUBLICATION'                                       
         MVC   FISHD2+20(11),=C'PUBLICATION'                                    
         MVC   FISHD2+40(11),=C'PUBLICATION'                                    
         MVC   FISHD2+60(11),=C'PUBLICATION'                                    
         B     HEAD15                                                           
*                                                                               
HEAD5    MVC   FISHD2(15),=C'PRD/PUBLICATION'                                   
         MVC   FISHD2+27(15),=C'PRD/PUBLICATION'                                
         MVC   FISHD2+54(15),=C'PRD/PUBLICATION'                                
         CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BE    HEAD10                                                           
         CLI   SVCLT,C'*'          SEE IF DOING OFFICE                          
         BE    HEAD10                                                           
         CLI   SVCLT,C'$'          SEE IF DOING OFFICE LIST                     
         BE    HEAD10                                                           
         CLC   SVCLT,=C'ALL'                                                    
         BNE   HEAD15                                                           
HEAD10   MVC   FISHD2(19),=C'CLT/PRD/PUBLICATION'                               
         MVC   FISHD2+27(19),=C'CLT/PRD/PUBLICATION'                            
         MVC   FISHD2+54(19),=C'CLT/PRD/PUBLICATION'                            
HEAD15   FOUT  FISHD2H                                                          
SCLEAR   LA    R4,14               CLEAR LOWER SCREEN                           
         LA    R3,FISOT01H                                                      
SCLEAR5  XC    8(80,R3),8(R3)                                                   
         FOUT  (R3)                                                             
         LA    R3,LINLEN(R3)                                                    
         BCT   R4,SCLEAR5                                                       
*                                                                               
         MVI   DATASW,0                                                         
*                                                                               
         CLC   LNAMESW,PNAMESW     SEE IF NAME DISPLAY STATUS CHANGED           
         BE    *+14                                                             
         XC    PREVKEY,PREVKEY     CLEAR LAST KEY                               
         MVI   PREVDSW,0           AND LAST DATA SWITCH                         
*                                                                               
         LA    R6,REC+1000         26 CHAR ELEMS                                
         LA    R6,3000(R6)                                                      
*****                                                                           
         CLI   PNAMESW,C'Y'        SEE IF DOING PUB NAMES                       
         BNE   RDFRST              NO                                           
         LA    R3,FISOT01H                                                      
         LA    R7,14               MAX FOR NAMES                                
         B     RDFRST5                                                          
RDFRST   LA    R7,42               MAX FOR ALL PRDS                             
         CLC   SVPRD,=C'ALL'                                                    
         BE    *+8                                                              
         LA    R7,56               MAX IS 56 FOR ONE PRD - 4 UP                 
*                                  BUILD FIRST KEY                              
RDFRST5  XC    KEY,KEY                                                          
         XC    WORKCLT,WORKCLT                                                  
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'20'                                                      
         CLI   SVCLT,C'&&'           BILLING GROUP                              
         BE    RDFRST7                                                          
         CLI   SVCLT,C'*'                                                       
         BE    RDFRST7                                                          
         CLI   SVCLT,C'$'            OFFICE LIST                                
         BE    RDFRST7                                                          
         CLC   SVCLT,=C'ALL'                                                    
         BE    RDFRST7                                                          
         MVC   KEY+4(3),SVCLT                                                   
RDFRST7  CLC   SVPRD,=C'ALL'                                                    
         BE    *+10                                                             
         MVC   KEY+7(3),SVPRD                                                   
         MVC   KEY+10(6),SVPUB     PUB TO START DISPLAY AT                      
         XC    LASTPUB,LASTPUB                                                  
         MVI   PUBPSW,0                                                         
         XC    LASTCP,LASTCP       CLEAR LAST CLT/PRD                           
*                                                                               
         OC    PREVKEY,PREVKEY     FIRST TIME                                   
         BZ    RDFRST9                                                          
         MVC   KEY,PREVKEY          NO - RESTORE PREV KEY                       
         MVC   DATASW,PREVDSW       SET DATASW FROM PREVDSW                     
         XC    PREVKEY,PREVKEY                                                  
*                                                                               
RDFRST9  DS    0H                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
*                                                                               
RHI      BAS   RE,HIGH                                                          
         B     HAVREC                                                           
*                                                                               
RSEQ     BAS   RE,SEQ                                                           
HAVREC   LA    R5,KEY                                                           
         CLC   KEY(4),KEYSAVE      AGY/MED REC TYPE                             
         BNE   REND                                                             
**NEW 4/5/91                                                                    
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         CLC   FATIOCNT,SVMAXIO                                                 
         BL    HAV                                                              
         MVC   PREVKEY,KEY        SO I WILL CONTINUE LOOKING                    
         B     REND                                                             
         DROP  R1                                                               
*                                                                               
HAV      DS    0H                                                               
         TM    KEY+25,X'C0'        SEE IF CLOSED OUT                            
         BO    RSEQ                YES SKIP                                     
         CLC   KEY+4(6),LASTCP     CHK FOR CHANGE OF CLT/PRD                    
         BE    HAVR                                                             
         XC    LASTPUB,LASTPUB     MUST CLEAR LASTPUB AND PUBPSW                
         MVI   PUBPSW,0                                                         
HAVR     CLC   SVCLT,=C'ALL'                                                    
         BE    HAVR1                                                            
         CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BE    HAVR07                                                           
         CLI   SVCLT,C'$'          SEE IF DOING OFFICE LIST                     
         BE    HAVR02                                                           
         CLI   SVCLT,C'*'          SEE IF DOING AN OFFICE                       
         BNE   HAVR0                                                            
HAVR02   CLC   WORKCLT,KEY+4       SEE IF I'VE CHECKED THIS CLIENT              
         BE    HAVR1                                                            
         BAS   RE,CHKACC           READS CLT AND USES VOFFICER                  
         BNE   RHI                 TO SEE IF THIS CLT SHOULD BE                 
*NOP*    B     HAVR1               PROCESSED                                    
*                                                                               
HAVR05   CLI   SVCLT,C'*'          SEE IF DOING AN OFFICE                       
         BNE   HAVR1                                                            
         BAS   RE,CHKOFF                                                        
         BNE   RHI                 CONDITION CODE NOT FOUND                     
         B     HAVR1                                                            
*                                  MEANS SKIP TO NEXT CLT                       
HAVR07   CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BNE   HAVR0                                                            
         CLC   WORKCLT,KEY+4       SEE IF I'VE CHKED THIS CLT                   
         BE    HAVR1               YES                                          
         BRAS  RE,CHKGRP                                                        
         BNE   RHI                 CONDITION CODE NOT FOUND                     
         B     HAVR1                                                            
*                                  MEANS SKIP TO NEXT CLT                       
HAVR0    CLC   KEY+4(3),SVCLT                                                   
         BNE   REND                                                             
HAVR1    CLC   SVPRD,=C'ALL'                                                    
         BE    HAVR2                                                            
         CLC   KEY+7(3),SVPRD                                                   
         BE    HAVR2                                                            
         CLI   SVCLT,C'*'          SEE IF OFFICE                                
         BE    HAVR1X                                                           
         CLI   SVCLT,C'$'          OFFICE LIST                                  
         BE    HAVR1X                                                           
         CLI   SVCLT,C'&&'         GROUP                                        
         BNE   REND                DONE                                         
*                                                                               
HAVR1X   DS    0H                  SHOULD GET ME TO NEXT CLT                    
         CLC   KEY+7(3),SVPRD                                                   
         BH    HAVR1X5                                                          
HAVR1X3  XC    KEY+7(25),KEY+7      MUST CLEAR KEY PAST CLT                     
         MVC   KEY+7(3),SVPRD                                                   
         B     RHI                                                              
*                                                                               
HAVR1X5  MVC   KEY+7(2),=2X'FF'    SKIP TO NEXT CLIENT                          
         XC    WORKCLT,WORKCLT                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(4),KEYSAVE      CHECK RIGHT AGY/MED/REC                      
         BNE   REND                                                             
         B     HAVR1X3             GO LOOK FOR MY PRD                           
*                                                                               
HAVR2    CLC   LASTPUB,KEY+10      CHK FOR SAME PUB                             
         BE    HAVR2B                                                           
         MVI   PUBPSW,0            NEW PUB-ZERO PUBPSW                          
*                                                                               
HAVR2B   DS    0H                                                               
         CLI   MTHSW,C'B'             SEE IF DOING BILLABLE DATES               
         BE    HAVR4                                                            
         CLC   KEY+16(2),SVSTRTB      CHK DATES - MTH ONLY                      
         BNL   HAVR3                                                            
         CLI   PRSW,1                 SEE IF DOING PRIOR                        
         BE    HAVR4                                                            
         MVC   KEY+16(3),SVSTRTB      READ HI FOR THIS DATE                     
         B     RHI                                                              
*                                                                               
HAVR3    CLC   KEY+16(2),SVENDB       CHK END - MONTH ONLY                      
         BNH   HAVR4                                                            
         CLI   SUBSW,1                SEE IF DOING SUBSEQUENT                   
         BE    HAVR4                                                            
HAVR3X   MVC   KEY+16(3),=3X'FF'      TO GET NEXT PUB                           
         B     RHI                                                              
*                                                                               
******   OC    KEY+21(3),KEY+21       NO PASSIVE - CHK NO-OPED                  
******   BNZ   RSEQ                                                             
*                                                                               
HAVR4    CLC   SVEST,=C'ALL'                                                    
         BE    HAVR4A                                                           
         CLC   KEY+19(2),SVESTB                                                 
         BNE   RSEQ                                                             
*                                                                               
HAVR4A   DS    0H                                                               
         CLI   MTHSW,C'B'         SEE IF DOING BILLABLE DATES                   
         BNE   HAVR5                                                            
*                                                                               
         BAS   RE,GETREC                                                        
         CLC   PBDBDATE(2),SVSTRTB                                              
         BNL   HAVR4B                                                           
         CLI   PRSW,1             SEE IF DOING PRIOR                            
         BE    HAVR4B                                                           
         B     RSEQ               SKIP                                          
*                                                                               
HAVR4B   CLC   PBDBDATE(2),SVENDB                                               
         BNH   HAVR5                                                            
         CLI   SUBSW,1            SEE IF DOING SUBSEQUENT                       
         BE    HAVR5                                                            
         B     RSEQ                                                             
*                                                                               
HAVR5    OC    KEY+21(3),KEY+21    CHK FOR PASSIVE KEY                          
         BZ    HAVR5B                                                           
         TM    PUBPSW,X'02'        SEE IF I'VE DONE PASSIVE                     
         BNZ   RSEQ                YES DON'T DO AGAIN                           
         OI    PUBPSW,X'02'                                                     
         B     HAVR5B5                                                          
*                                                                               
HAVR5B   TM    PUBPSW,X'01'        SEE IF I'VE DONE NON-PASSIVE                 
         BNZ   RSEQ                YES DON'T DO AGAIN                           
         OI    PUBPSW,X'01'                                                     
*                                                                               
HAVR5B5  CLI   PNAMESW,C'Y'        SEE IF DOING PUB NAMES                       
         BNE   *+8                                                              
         LA    R6,8(R3)                                                         
         MVC   0(3,R6),KEY+4       CLT CODE                                     
         MVI   3(R6),C'/'                                                       
         LR    R5,R6                                                            
         CLI   SVCLT,C'&&'         SEE IF DOING BILLING GROUP                   
         BE    HAVR50                                                           
         CLI   SVCLT,C'*'          SEE IF DOING OFFICE                          
         BE    HAVR50                                                           
         CLI   SVCLT,C'$'          SEE IF DOING OFFICE LIST                     
         BE    HAVR50                                                           
         CLC   SVCLT,=C'ALL'       OR ALL CLIENTS                               
         BNE   *+8                                                              
HAVR50   LA    R5,4(R5)                                                         
         MVC   0(3,R5),KEY+7       PRD CODE                                     
         MVI   3(R5),C'/'                                                       
         CLC   SVPRD,=C'ALL'                                                    
         BNE   *+8                                                              
         LA    R5,3(R5)                                                         
         OC    KEY+21(3),KEY+21    SEE IF PASSIVE POINTER                       
         BZ    HAVR53                                                           
         MVI   0(R5),C'*'          YES DENOTE WITH '*'                          
*                                  BEFORE PUB NUMBER                            
*                                  INSTEAD OF '/'                               
         LA    R5,1(R5)                                                         
         B     HAVR54                                                           
*                                                                               
HAVR53   CLI   0(R5),C'/'                                                       
         BNE   HAVR54                                                           
         LA    R5,1(R5)            BUMP PAST '/'                                
HAVR54   ZIC   R4,SVAPROF+12                                                    
         GOTO1 =V(PUBEDIT),DMCB,((R4),KEY+10),0(R5),RR=RELO                     
         CLI   PNAMESW,C'Y'        SEE IF DOING PUB NAMES                       
         BNE   HAVR5M              NO                                           
         MVI   DATASW,X'01'                                                     
         MVC   WORK(L'KEY),KEY     SAVE BUY KEY                                 
         XC    KEY,KEY                                                          
         MVC   KEY(1),SVMED        READ PUB                                     
         MVC   KEY+1(6),WORK+10      PUB                                        
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
         BAS   RE,HIGHPUB                                                       
HAVR5A   CLC   KEY(7),KEYSAVE                                                   
         BNE   HAVR5CX             NOT FOUND                                    
         CLC   KEY(10),KEYSAVE                                                  
         BE    HAVR5D                                                           
         CLI   SVAPROF+16,C'0'                                                  
         BE    HAVR5C                                                           
         CLC   KEY+7(2),=C'ZZ'     SEE IF I FOUND DEFAULT                       
         BE    HAVR5D                                                           
HAVR5C   BAS   RE,SEQPUB                                                        
         B     HAVR5A                                                           
HAVR5CX  DC    H'0'                PUB NOT ON FILE FATAL ERROR                  
*                                                                               
HAVR5D   LA    R8,REC                                                           
         ST    R8,APUBIO                                                        
         BAS   RE,GETPUB                                                        
         CLC   SVPRD,=C'ALL'                                                    
         BE    HAVR5D5             FOR ALL PRDS THIS WOULD BE AN                
*                                  * PRODUCT                                    
         CLI   0(R6),C'*'          R6 POINTS TO 8(R3)                           
         BNE   HAVR5D5             IF FIELD STARTED WITH *                      
         BCTR  R5,0                I MUST DECREMENT R5                          
*                                  ELSE PUBNAME WILL BE IN WRONG COL.           
*                                                                               
HAVR5D5  MVC   20(20,R5),PUBNAME                                                
         CLI   SVMED,C'N'                                                       
         BE    HAVR5E                                                           
         MVC   42(20,R5),PUBZNAME                                               
         FOUT  (R3)                                                             
         LA    R3,LINLEN(R3)                                                    
         B     HAVR5G                                                           
*                                                                               
HAVR5E   MVC   42(16,R5),PUBCITY                                                
         MVC   60(2,R5),PUBSTATE                                                
         FOUT  (R3)                                                             
         LA    R3,LINLEN(R3)                                                    
         OC    PUBZNAME,PUBZNAME   CHK FOR ZONE NAME                            
         BZ    HAVR5G                                                           
         LA    R5,LINLEN(R5)       MUST GO ON NEXT LINE                         
         MVC   20(20,R5),PUBZNAME                                               
         FOUT  (R3)                                                             
         LA    R3,LINLEN(R3)                                                    
         BCTR  R7,0                DECREMENT R7 FOR ZONE LINE                   
*                                                                               
HAVR5G   MVC   KEY,WORK            RESTORE BUY KEY                              
         GOTO1 HIGH                                                             
         CLI   SVMED,C'N'                                                       
         BNE   HAVR5H                                                           
         CH    R7,=H'3'            NEWSPAPERS MAY NEED 2 LINES                  
         BNL   HAVR5H              AFTER BCT                                    
         B     HAVR6B                                                           
HAVR5H   BCT   R7,HAVR5X           GO FIND NEXT PUB                             
         B     HAVR6B                                                           
*                                                                               
HAVR5M   LA    R6,26(R6)                                                        
         MVI   0(R6),0             JUST IN CASE                                 
         BCT   R7,HAVR5X                                                        
         B     HAVR6B                                                           
*                                                                               
HAVR5X   TM    PUBPSW,X'03'        SEE IF I'VE BOTH PASSIVE AND                 
*                                  NON-PASSIVE                                  
         BO    HAVR5Z              YES SKIP TO NEXT PUB                         
         MVC   LASTCP,KEY+4        SAVE CLT/PRD                                 
         MVC   LASTPUB,KEY+10      SAVE PUB                                     
         B     RSEQ                ELSE CONTINUE SEQ READ                       
*                                                                               
HAVR5Z   MVC   KEY+16(3),=3X'FF'      TO GET NEXT PUB                           
         MVI   PUBPSW,0                                                         
         B     RHI                                                              
*                                                                               
HAVR6B   MVC   PREVKEY,KEY                                                      
         MVC   PREVKEY+16(3),=3X'FF'                                            
*                                                                               
REND     MVI   DMINBTS,X'C0'       RESET DMINBTS                                
         CLI   PNAMESW,C'Y'        SEE IF DOING PUB NAMES                       
         BE    FRMTEND             YES - DONE                                   
*****                                                                           
         LA    R2,REC+1000                                                      
         LA    R2,3000(R2)                                                      
         CLI   0(R2),0                                                          
*        CLI   REC+1000,0                                                       
*                                                                               
         BNZ   FORMAT                                                           
         B     FRMTEND                                                          
*                                                                               
FORMAT   XC    DMWORK(20),DMWORK                                                
         LA    R6,REC+1000                                                      
         LA    R6,3000(R6)                                                      
*****                                                                           
         LA    R7,0                USED TO COUNT ENTRIES                        
FORMAT1  CLI   0(R6),0                                                          
         BE    FORMAT2                                                          
         LA    R6,26(R6)                                                        
         LA    R7,1(R7)                                                         
         MVI   DATASW,X'01'    SET DATA FOUND                                   
         B     FORMAT1                                                          
*                                                                               
FORMAT2  CLC   SVPRD,=C'ALL'                                                    
         BNE   FORMAT10                                                         
*****                                                                           
         LA    R6,REC+1000                                                      
         LA    R6,3000(R6)                                                      
         GOTO1 =A(FMTALPH),DMCB,(26,(R6)),(R7),14,(3,DMWORK),RR=RELO            
*        GOTO1 =A(FMTALPH),DMCB,(26,REC+1000),(R7),14,(3,DMWORK),               
*              RR=RELO                                                          
*                                                                               
         LA    R2,FISOT01H                                                      
FORMAT3  LA    R6,DMWORK                                                        
         LA    RF,8(R2)                                                         
         CLI   0(R6),0                                                          
         BE    FRMTEND                                                          
FORMAT4  CLI   0(R6),0                                                          
         BE    FRMTSEND                                                         
         L     R7,0(R6)                                                         
         MVC   0(26,RF),0(R7)                                                   
         SR    RE,RE               DECREMENT COUNT                              
         IC    RE,0(R6)                                                         
         BCTR  RE,0                                                             
         L     R5,0(R6)                                                         
         LA    R5,26(R5)                                                        
         ST    R5,0(R6)                                                         
         STC   RE,0(R6)                                                         
         LA    R6,4(R6)            NEXT COLUMN                                  
         LA    RF,27(RF)                                                        
         B     FORMAT4                                                          
*                                                                               
FRMTSEND FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     FORMAT3                                                          
*                                                                               
FORMAT10 DS    0H                                                               
*****                                                                           
         LA    R6,REC+1000                                                      
         LA    R6,3000(R6)                                                      
         GOTO1 =A(FMTALPH),DMCB,(26,(R6)),(R7),14,(4,DMWORK),RR=RELO            
*        GOTO1 =A(FMTALPH),DMCB,(26,REC+1000),(R7),14,(4,DMWORK),               
*              RR=RELO                                                          
*                                                                               
         LA    R2,FISOT01H                                                      
FORMAT13 LA    R6,DMWORK                                                        
         LA    RF,8(R2)                                                         
         CLI   0(R6),0                                                          
         BE    FRMTEND                                                          
FORMAT14 CLI   0(R6),0                                                          
         BE    FORMAT18                                                         
         L     R7,0(R6)                                                         
         MVC   0(19,RF),0(R7)                                                   
         SR    RE,RE               DECREMENT COUNT                              
         IC    RE,0(R6)                                                         
         BCTR  RE,0                                                             
         L     R5,0(R6)                                                         
         LA    R5,26(R5)                                                        
         ST    R5,0(R6)                                                         
         STC   RE,0(R6)                                                         
         LA    R6,4(R6)            NEXT COLUMN                                  
         LA    RF,20(RF)                                                        
         B     FORMAT14                                                         
*                                                                               
FORMAT18 FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         B     FORMAT13                                                         
*                                                                               
FRMTEND  LA    R2,FISPUBH                                                       
         MVC   FISEMSG,=CL60'** NO DATA FOUND **'                               
         CLI   DATASW,0    (COULD BE FROM PREVDSW ON CONTINUATIONS)             
         BE    MODEXIT                                                          
         MVC   FISEMSG,=CL60'** END OF REQUESTED DATA **'                       
         OC    PREVKEY,PREVKEY                                                  
         BZ    MODEXIT                                                          
         MVC   PREVDSW,DATASW      SAVE DATASW                                  
         LA    R2,FISEND1H                                                      
         OI    FISOPTH+1,X'01'     MODIFY OPTION FIELD FOR NEXT PAGE            
         FOUT  FISOPTH                                                          
         MVC   FISEMSG,=CL60'** HIT ENTER FOR NEXT PAGE **'                     
MODEXIT  OI    6(R2),X'C0'                                                      
         MVC   LNAMESW,PNAMESW     SAVE NAME SWITCH                             
         FOUT  FISEMSGH                                                         
         XMOD1                                                                  
         EJECT                                                                  
*                                  NEED TO CHK CLIENT OFFICE                    
CHKOFF   NTR1                                                                   
         MVC   WORK(32),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(3),WORK         AGY/MED                                      
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),WORK+4                                                  
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
         CLI   SVCLT+1,C'-'        SEE IF DOING ALL BUT                         
         BNE   CKO4                                                             
         CLC   PCLTOFF(1),SVCLT+2                                               
         BE    CKONX               SKIP TO NEXT CLT                             
         B     CKOX                                                             
*                                                                               
CKO4     CLI   SVCLT+2,0           SEE IF DOING A RANGE                         
         BNH   CKO8                NO                                           
*                                                                               
*        TRANSLATE PCLTOFF TO 2 CH OFFICE CODE                                  
*                                                                               
         XC    WORK2,WORK2        WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK2           (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF      CLT OR CLT TRAFFIC OFFICE CODE               
         MVC   OFCCLT,PCLTKCLT                                                  
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT,6(RA)                                                     
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD          A("SECRET BLOCK")                            
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',WORK2),ACOMFACS                              
         CLI   0(R1),0                                                          
         BNE   CKONX               SKIP CLIENT IF INVALID OFFICE CODE           
*                                                                               
         LA    R1,WORK2            ESTABLISH OFFICER CONTROL BLOCK              
         USING OFFICED,R1                                                       
*                                                                               
         CLC   OFCOFC2,SVOFCST2    MAKE SURE OFFICE IN RANGE                    
         BL    CKONX               LOW SKIP                                     
         CLC   OFCOFC2,SVOFCEN2                                                 
         BH    CKONX               HIGH SKIP                                    
*                                                                               
         B     CKOX                PROCESS                                      
*                                                                               
         DROP  R1                                                               
*                                                                               
CKO8     CLC   PCLTOFF(1),SVCLT+1  ELSE CODES MUST MATCH                        
         BE    CKOX                                                             
*                                                                               
CKONX    MVC   KEY,WORK            RESTORE KEY                                  
         MVC   KEY+7(2),=2X'FF'    TO GET ME TO NEXT CLIENT                     
         XC    WORKCLT,WORKCLT                                                  
         LTR   RE,RE               SET CONDITION CODE NE                        
         B     CKOXX               WHEN RETURNS WILL GO TO RHI                  
*                                                                               
CKOX     MVC   WORKCLT,KEY+4                                                    
         MVC   KEY,WORK            RESTORE KEY                                  
         BAS   RE,HIGH             NEED TO RESTORE SEQ READ                     
         CLI   SVCLT,C'*'          TO SET CONTITION CODE EQ                     
CKOXX    XIT1                                                                   
         EJECT                                                                  
*                                  NEED TO CHK BILLING GROUP                    
*                                  HERE IF SVCLT = $ OR *                       
*                                  NEED TO CHK CLIENT OFFICE                    
CHKACC   NTR1                      VS OFFICE LIST LIMIT ACCESS                  
         MVC   WORK(32),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(3),WORK         AGY/MED                                      
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),WORK+4                                                  
         BAS   RE,READ                                                          
         BAS   RE,GETREC                                                        
*                                                                               
         CLI   TRFAGSW,C'Y'        TRAFFIC AGENCY ID ?                          
         BNE   EDTC30              NO                                           
*                                 SEE IF TRAFFIC OFFICE EXISTS                  
         LA    R6,PCLTREC+33                                                    
EDTC20   CLI   0(R6),0            END OF REC ?                                  
         BE    EDTC30             YES                                           
         CLI   0(R6),X'50'        CLT TRAFFIC OFFICE ELEM CODE                  
         BE    EDTC25             YES                                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0              BUMP TO NEXT ELEM                             
         B     EDTC20                                                           
EDTC25   MVC   PCLTOFF,2(R6)    REPLACE CLT OFFICE WITH TRAFFIC OFFICE          
EDTC30   DS    0H                                                               
*                                                                               
CHKACC5  BAS   RE,PPCLIVER                                                      
         BE    CKAX               PROCESS                                       
         B     CKANX               SKIP THIS CLIENT                             
*                                                                               
CKANX    MVC   KEY,WORK            RESTORE KEY                                  
         MVC   KEY+7(2),=2X'FF'    TO GET ME TO NEXT CLIENT                     
         XC    WORKCLT,WORKCLT                                                  
         LTR   RE,RE               SET CONDITION CODE NE                        
         B     CKAXX               WHEN RETURNS WILL GO TO RHI                  
*                                                                               
CKAX     MVC   WORKCLT,KEY+4                                                    
         MVC   KEY,WORK            RESTORE KEY                                  
         BAS   RE,HIGH             NEED TO RESTORE SEQ READ                     
*NOP*    CLI   SVCLT,C'$'          TO SET CONTITION CODE EQ                     
         CR    RE,RE               TO SET CONDITION CODE EQ                     
CKAXX    XIT1                                                                   
         EJECT                                                                  
*       *************************                                               
******  TEST OFFICE LIST SECURITY  ******                                       
*       *************************                                               
*                                                                               
*        ONLY GO TO THIS ROUTINE IF SVCLT IS $N - OFFICE LIST                   
*                                                                               
         SPACE 2                                                                
*NOP*PPCLIVER NTR1 ***** NOTE- I/O AREA M/B IN AREC ******                      
         SPACE 2                                                                
*                   MUST USE WORK+35 SINCE CHKACC USE WORK (32)                 
*                                                                               
*NOP*    XC    WORK+35(10),WORK+35                                              
*NOP*    LA    R1,WORK+35                                                       
*NOP*    USING OFFICED,R1                                                       
*NOP*    MVI   OFCSYS,C'P'                                                      
*NOP*    MVC   OFCAUTH,SVCLT       SVCLT SHOULD HAVE $N - OFFICE LIST           
*NOP*    MVC   OFCAGY,AGYALPHA                                                  
*NOP*    MVC   OFCOFC,PCLTOFF                                                   
*NOP*    DROP  R1                                                               
*NOP*    GOTO1 VOFFICER,DMCB,WORK+35,ACOMFACS                                   
*NOP*    CLI   0(R1),0                                                          
*NOP*    XIT1                                                                   
*******************************************************                         
         EJECT                                                                  
*                                                                               
PPCLIVER NTR1                   *****  LIMIT ACCESS TESTING   *****             
         XC    WORK2,WORK2        WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK2           (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         CLI   SVCLT,C'$'         DOING OFFICE LIST ?                           
         BNE   *+10                                                             
         MVC   OFCAUTH,SVCLT                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF     CLT OR CLT TRAFFIC OFFICE CODE                
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RA)                                                  
         CLI   SVCLT,C'$'         DOING OFFICE LIST ?                           
         BNE   *+10                                                             
         MVC   OFCLMT(2),SVCLT                                                  
         LA    R0,SECBLK                                                        
         ST    R0,OFCSECD         A("SECRET BLOCK")                             
         DROP  R1                                                               
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',WORK2),ACOMFACS                              
         CLI   0(R1),0                                                          
         XIT1                                                                   
         EJECT                                                                  
STOP     DS    0H                                                               
         XC    FISEMSG,FISEMSG                                                  
         MVC   FISEMSG(49),=C'**MAXIMUM FILE READS EXCEEDED - REQUEST SX        
               TOPPED**'                                                        
         FOUT  FISEMSGH                                                         
         B     EXXMOD                                                           
*                                                                               
       ++INCLUDE PPGENEROL                                                      
         LTORG                                                                  
CHKGRP   NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(32),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(3),WORK         AGY/MED                                      
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),WORK+4                                                  
         BRAS  RE,READ                                                          
         BRAS  RE,GETREC                                                        
         CLC   PCLTBLGP,SVCLT+1                                                 
         BE    CKGX                                                             
*                                                                               
CKGNX    MVC   KEY,WORK            RESTORE KEY                                  
         MVC   KEY+7(2),=2X'FF'    TO GET ME TO NEXT CLIENT                     
         XC    WORKCLT,WORKCLT                                                  
         LTR   RE,RE               SET CONDITION CODE NE                        
         B     CKGXX               WHEN RETURNS WILL GO TO RHI                  
*                                                                               
CKGX     MVC   WORKCLT,KEY+4                                                    
         MVC   KEY,WORK            RESTORE KEY                                  
         BRAS  RE,HIGH             NEED TO RESTORE SEQ READ                     
         CLI   SVCLT,C'&&'         TO SET CONTITION CODE EQ                     
CKGXX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
FMTALPH  CSECT                                                                  
         NMOD1 0,FRMTALPH                                                       
*                                                                               
* FORMAT TABLE IN ALPHABETIC COLUMNS                                            
*                                                                               
*        PARAMETER 1     BYTE   0 = L'EACH ENTRY                                
*                        BYTE 1-3 = A(TABLE)                                    
*        PARAMETER 2     BYTE 0-3 = NUMBER OF ENTRIES IN TABLE                  
*        PARAMETER 3     BYTE 0-3 = NUMBER OF ROWS TO BE FORMATTED              
*                                                                               
*        PARAMETER 4     BYTE   0 = NUMBER OF COLUMNS TO BE FORMATTED           
*                        BYTE 1-3 = A(FORMAT LIST)                              
*                                                                               
*        OUTPUT FORMAT LIST (ONE ENTRY FOR EACH COLUMN)                         
*                BYTE  0 = NUMBER OF ENTRIES FOR THIS COLUMN                    
*                BYTE 1-3= A(START FIELD FOR THIS COLUMN)                       
*                                                                               
*               A(TABLE) IS SET TO NEXT ENTRY POINT                             
*               NUMBER OF ENTRIES IS SET TO NUMBER OF ENTRIES LEFT IN           
*                TABLE.                                                         
*                                                                               
         L     R6,12(R1)           CLEAR FORMAT LIST                            
         LA    R6,0(R6)                                                         
         SR    R7,R7                                                            
         IC    R7,12(R1)                                                        
         XC    0(4,R6),0(R6)                                                    
         LA    R6,4(R6)                                                         
         BCT   R7,*-10                                                          
*                                                                               
         SR    RF,RF               GET END OF TABLE                             
         IC    RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         MH    RF,6(R1)            NUMBER OF ENTRIES X ENTRY LENGTH             
         L     R6,0(R1)                                                         
         LA    RF,0(RF,R6)         END OF TABLE                                 
*                                                                               
         LA    R6,0(R6)            A(TABLE START)                               
         L     R7,12(R1)           A(FORMAT LIST)                               
         SR    R8,R8                                                            
         IC    R8,12(R1)           NUMBER OF COLUMNS                            
FA1      ST    R6,0(R7)            SAVE COLUMN ADDRESS                          
         SR    R4,R4                                                            
         CLC   4(4,R1),8(R1)       NUMBER OF ENTRIES GT NO OF ROWS              
         BH    FA2                                                              
         MVC   0(1,R7),7(R1)        NO - SET ROWS FOR THIS COLUMN               
         XC    4(4,R1),4(R1)       SET ENTRIES LEFT TO ZERO                     
         LR    R6,RF                                                            
         B     FAEX                                                             
FA2      MVC   0(1,R7),11(R1)      SET TO NUMBER OF ROWS                        
         SR    R9,R9                                                            
         IC    R9,0(R1)                                                         
         MH    R9,10(R1)           X ENTRY LENGTH                               
         LA    R6,0(R9,R6)         SET TO NEXT COLUMN ADDRESS                   
         LA    R7,4(R7)                                                         
         L     R9,4(R1)            DECREMENT NUMBER OF ENTRIES                  
         S     R9,8(R1)                                                         
         ST    R9,4(R1)            SAVE ENTRIES LEFT                            
         BCT   R8,FA1              GET NEXT COLUMN ADDRESS                      
*                                                                               
FAEX     IC    RF,0(R1)            SAVE A(NEXT START)                           
         ST    R6,0(R1)                                                         
         STC   RF,0(R1)                                                         
         B     FMTALPX                                                          
*                                                                               
FMTALPX  XMOD1 1                                                                
         LTORG                                                                  
       ++INCLUDE PPFISWRK                                                       

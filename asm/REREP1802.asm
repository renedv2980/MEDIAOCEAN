*          DATA SET REREP1802  AT LEVEL 018 AS OF 07/27/07                      
*          DATA SET REREP1802  AT LEVEL 001 AS OF 07/24/91                      
*PHASE RE1802A,*                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'REREP1802(RE1802) --- ACE/TWX/OTHER CONTRACT SWITCHER'          
*                                                                               
**********************************************************************          
*                                                                    *          
*        REREP1802(RE1802)  --- ACE/TWX/OTHER CONTRACT SWITCHER      *          
*                                                                    *          
**********************************************************************          
*                                                                    *          
*  ***   THIS IS A FILE MARKER   ***                                 *          
*  *** DO NOT RUN WHEN FAC IS UP ***                                 *          
*                                                                    *          
**********************************************************************          
*                                                                    *          
*  READ CONTRACTS FOR A STATION AND SET MODE TO ACE, TWX(WU/GRAPH),  *          
*  OR OTHER, BASED ON REQUEST CARD.                                  *          
*                                                                    *          
*  CARD INPUT:                                                       *          
*                                                                    *          
*  QREP     -- REP POWER CODE                                        *          
*  QSTATION -- STATION CALL LETTERS                                  *          
*  QOPTION1 -- 'CHANGE FROM' OPTION (READ FILTER)                    *          
*          A = FROM ACE                                              *          
*          T = FROM TWX/WU/GRAPHNET                                  *          
*          O = FROM OTHER (ACTUALLY, NEITHER)                        *          
*                                                                    *          
*  QOPTION2 -- 'CHANGE TO' OPTION                                    *          
*          A = SET CONTRACTS TO ACE                                  *          
*          T = SET CONTRACTS TO TWX/WU/GRAPHNET                      *          
*          O = SET CONTRACTS TO OTHER (ACTUALLY, SET TO NEITHER)     *          
*                                                                    *          
*  QOPTION4 -- T = TEST ONLY (NO UPDATE)                             *          
*                  (THIS OPTION NOT ON THE SCREEN)                   *          
*                                                                    *          
*  ALL PROCESSING DONE IN REQ FIRST MODE.                            *          
*                                                                    *          
*  REPORT RECORD AND UPDATE COUNTS.                                  *          
*                                                                    *          
**********************************************************************          
*                                                                    *          
*  MOD LOG                                                           *          
*  -------                                                           *          
*                                                                    *          
*  01/18/90  PJS  ORIGINAL DEVELOPMENT                               *          
*                                                                    *          
*  JUL22/91 (MRR) --- PERFORM ALL FUNCTIONS REQUIRED FOR SWITCHING   *          
*                      FROM OTHER TO TWX                             *          
*                                                                    *          
*  DEC08/92 (SKU) --- TYPE 'OTHER' IS ACTUALLY NOT ACE NOR GRAPHNET  *          
*                                                                    *          
*  JUN21/93 (SKU) --- FIX BUG TO CLEAR 'FROM' BIT                    *          
*                                                                    *          
*  OCT25/94 (SKU) --- FORCE PAGE BREAK SO FOOTER FITS ON SAME PAGE   *          
*                                                                    *          
*  APR05/95 (SKU) --- UPDATE FOR NEW X'20' ELEMENT                   *          
*                     ALSO REMOVE X'20' DEFAULT BIT IN RCONSENF      *          
*                                                                    *          
*  APR01/96 (SKU) --- SUPPORT FOR 1976-BYTE CONTRACT                 *          
*                                                                    *          
*  JUL27/07 (SKU) --- CHANGE TO ADD COPY TO RECOVERY OUTPUT          *          
**********************************************************************          
*                                                                               
         PRINT NOGEN                                                            
RE1802   CSECT                                                                  
         NMOD1 0,**RE1802,RA,RR=R5                                              
         ST    R5,RELO                                                          
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
*                                                                               
*- CHECK FOR VALID PROCESSING MODE.                                             
         CLI   MODE,REQFRST                                                     
         BE    MAIN                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         SPACE 2                                                                
MAIN     EQU   *                                                                
         BAS   RE,INITIAL          STARTUP STUFF                                
         BNZ   EXIT                                                             
*                                                                               
* SET TO GENERATE RECOVERY OFFLINE COPIES                                       
* SINCE DXTRACT TO MO REQUIRES COPIES                                           
*                                                                               
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     R4,MASTC                                                         
         USING MASTD,R4                                                         
         DROP  RF                                                               
         L     R6,MCSSB                                                         
         USING SSBOFFD,R6                                                       
         OI    SSOSTAT2,SSOSROLC                                                
         DROP  R4,R6                                                            
*                                                                               
*- MAIN LOOP                                                                    
MAIN100  EQU   *                   GET CONTRACT                                 
         BAS   RE,NEXTCON          GET CONTRACT                                 
         BNZ   MAIN200             END                                          
*                                                                               
         BAS   RE,SWITCH           SWITCH STATUS                                
         BNZ   MAIN190             NO UPDATE.                                   
         BAS   RE,DODETAIL                                                      
*                                                                               
*- WRITE BACK TO FILE OR JUST A TEST?                                           
         CLI   QOPTION4,C'T'                                                    
         BE    MAIN190                                                          
*                                                                               
         GOTO1 PREC                WRITE BACK TO FILE                           
*                                                                               
MAIN190  B     MAIN100                                                          
*                                                                               
*- END OF RUN -- PRINT OUT RECORD COUNTS                                        
MAIN200  EQU   *                                                                
         BAS   RE,DOREPORT                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*- REPORT INITIAL                                                               
INITIAL  NTR1                                                                   
         MVI   FOOTSW,C'N'                                                      
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,X'99'                                                       
*                                                                               
         MVC   SENDTIME(4),=P'80000'                                            
***      TIME                                                                   
***      N     R0,=X'FFFFFFF0'                                                  
***      O     R0,=X'0000000C'                                                  
***      STCM  R0,15,SENDTIME             SAVE TIME FOR SEND ELEMENT            
***      SRP   SENDTIME(4),62,0           'DIVIDE' BY 100                       
***      AP    SENDTIME(4),=P'080000'     DDS TIME TO 'REAL' TIME               
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY2BT)     COMPRESSED DATE          
*                                                                               
         XC    COUNT(LCOUNT),COUNT 0 COUNTERS                                   
         XC    KEY,KEY             1ST PASS SW.                                 
*                                                                               
         LA    RE,IOAREA                                                        
         ST    RE,AIOAREA                                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'REP',=C'NCTFILE X'                      
         BAS   RE,GETSSID                                                       
         BNZ   INITBAD                                                          
*                                                                               
*- PRINT SOME BASIC REPORT INFO.                                                
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    R2,P                                                             
         USING INFOLINE,R2                                                      
*                                                                               
         MVC   ICONMSG,CONMSG                                                   
         MVC   ISTA,QSTATION                                                    
         CLI   QSTATION+4,C' '     ANY BAND?                                    
         BE    INIT100                                                          
         MVI   IDASH,C'-'                                                       
         MVC   IBAND,QSTATION+4                                                 
INIT100  EQU   *                                                                
         MVC   ISWIMSG,SWIMSG      SWITCH FROM                                  
*                                                                               
         LA    R1,ISWIMSG+L'SWIMSG                                              
*                                                                               
         LA    RE,LACE                                                          
         LA    RF,(L'LACE)-1                                                    
         CLI   QOPTION1,ACECODE    ACE?                                         
         BE    INIT120                                                          
*                                                                               
         LA    RE,LTWX                                                          
         LA    RF,(L'LTWX)-1                                                    
         CLI   QOPTION1,TWXCODE    TWX?                                         
         BE    INIT120                                                          
*                                                                               
         LA    RE,LOTH                                                          
         LA    RF,(L'LOTH)-1                                                    
         CLI   QOPTION1,OTHRCODE   OTHER?                                       
         BE    INIT120                                                          
*                                                                               
         DC    H'0'                INVALID OPTION 1                             
INIT120  EQU   *                                                                
         EX    RF,INIT150          SWITCH FROM XXX                              
*                                                                               
         AR    R1,RF               SWITCH FROM XXX TO                           
         MVC   1(L'TO,R1),TO                                                    
         LA    R1,1+L'TO(R1)                                                    
*                                                                               
         LA    RE,LACE                                                          
         LA    RF,(L'LACE)-1                                                    
         CLI   QOPTION2,ACECODE    ACE?                                         
         BE    INIT130                                                          
*                                                                               
         LA    RE,LTWX                                                          
         LA    RF,(L'LTWX)-1                                                    
         CLI   QOPTION2,TWXCODE    TWX?                                         
         BE    INIT130                                                          
*                                                                               
         LA    RE,LOTH                                                          
         LA    RF,(L'LOTH)-1                                                    
         CLI   QOPTION2,OTHRCODE   OTHER?                                       
         BE    INIT130                                                          
*                                                                               
         DC    H'0'                INVALID OPTION 2                             
INIT130  EQU   *                                                                
         EX    RF,INIT150          SWITCH FROM XXX TO YYY                       
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT              BLANK LINE                                   
         XC    KEY,KEY                                                          
*                                                                               
*        INIT EXIT                                                              
*                                                                               
INITGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
INITBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
*                                                                               
INIT150  MVC   0(0,R1),0(RE)       ** EXECUTED **                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*- NEXTCON -- READ CONTRACT VIA 'CC' KEY                                        
*             CC ^0 = EOF                                                       
NEXTCON  NTR1                                                                   
*                                                                               
         CLI   KEY,0               0 ON 1ST KEY                                 
         BNE   NEXT20                                                           
*                                                                               
         MVI   KEY,X'CC'           KEY ID                                       
         MVC   KEY+1(2),QREP       REP CODE                                     
         MVC   KEY+3(5),QSTATION   CALL LETTER                                  
         GOTO1 HIGH                                                             
         B     NEXT40                                                           
*                                                                               
NEXT20   EQU   *                                                                
         GOTO1 SEQ                                                              
*                                                                               
*- WE'RE DONE ON STATION BREAK.                                                 
NEXT40   EQU   *                                                                
         CLC   KEY(8),KEYSAVE      THRU STATION CALL LETTERS                    
         BNE   NEXTEOF                                                          
*                                                                               
         L     RE,NUMKEYS          COUNT # KEYS READ IN                         
         AH    RE,=H'1'                                                         
         ST    RE,NUMKEYS                                                       
*                                                                               
         GOTO1 GREC                READ IN CONTRACT                             
*                                                                               
*- INPUT FILTER: ONLY ACCEPT CONTRACTS MATCHING THE 'FROM' OPTION               
*                                                                               
         CLI   QOPTION1,ACECODE    FROM ACE?                                    
         BNE   NEXT50                                                           
         TM    RCONMODR+1,ACEMASK                                               
         BZ    NEXT20              NOT ACE                                      
*                                                                               
NEXT50   CLI   QOPTION1,TWXCODE    FROM TWX/WU/GRAPH                            
         BNE   NEXT60                                                           
         TM    RCONMODR+1,TWXMASK                                               
         BZ    NEXT20              NOT TWX                                      
*                                                                               
NEXT60   CLI   QOPTION1,OTHRCODE   FROM OTHER                                   
         BNE   NEXTGOOD                                                         
         TM    RCONMODR+1,ACEMASK+TWXMASK                                       
         BNZ   NEXT20              NOT OTHER                                    
*                                                                               
NEXTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
NEXTEOF  EQU   *                                                                
         LA    R0,1                ^0 = END OF CONTRACTS                        
         B     TESTEXIT                                                         
         EJECT                                                                  
*******************************************************************             
* PROCESS CONTRACT RECORDS                                        *             
*                                                                 *             
*    ACE CONTRACT - RCONMODR+1=X'80'                              *             
*    GRAPHNET CONTRACT - RCONMODR+1=X'40'                         *             
*    NEITHER ACE OR GRAPHNET CONTRACT - RCONMODR+1=X'FF'-X'C0'    *             
*                                                                 *             
*******************************************************************             
         SPACE                                                                  
SWITCH   NTR1                                                                   
*                                                                               
         MVC   SAVEMODE(1),RCONMODR+1                                           
*                                                                               
*- SPLIT ON TYPE OF SWITCH...ACE/TWX/OTHER                                      
*                                                                               
         CLI   QOPTION2,ACECODE    ACE                                          
         BE    SW100                                                            
         CLI   QOPTION2,TWXCODE    TWX/WU/GRAPHNET                              
         BE    SW200                                                            
         CLI   QOPTION2,OTHRCODE                                                
         BE    SW300                                                            
         DC    H'0'                INV. OPTION IN REQUEST                       
         SPACE                                                                  
*                                                                               
*- SWITCH TO ACE.  (RCONMODR+1 = X'80')                                         
SW100    EQU   *                                                                
         TM    RCONMODR+1,ACEMASK  ALREADY ACE?                                 
         BO    SWSKIP                                                           
*                                                                               
SW120    DS    0H                                                               
         OI    RCONMODR+1,ACEMASK             MAKE IT ACE.                      
         NI    RCONMODR+1,X'FF'-TWXMASK       CLEAR TWX BIT                     
         TM    SAVEMODE,ACEMASK+TWXMASK                                         
         BNZ   SW500                                                            
         BAS   RE,MAKE20EL                                                      
         BNZ   SWSKIP                                                           
         BAS   RE,UPDATE1F                                                      
         BNZ   SWSKIP                                                           
         BAS   RE,MAKE82EL                                                      
         BNZ   SWSKIP                                                           
         B     SW500                                                            
*                                                                               
*- SWITCH TO TWX.  (RCONMODR+1 = X'40')                                         
SW200    EQU   *                                                                
         TM    RCONMODR+1,TWXMASK          ALREADY TWX?                         
         BO    SWSKIP                                                           
*                                                                               
SW220    DS    0H                                                               
         OI    RCONMODR+1,TWXMASK          MAKE IT TWX.                         
         NI    RCONMODR+1,X'FF'-ACEMASK    CLEAR ACE BIT                        
         TM    SAVEMODE,ACEMASK+TWXMASK                                         
         BNZ   SW500                                                            
         BAS   RE,MAKE20EL                                                      
         BNZ   SWSKIP                                                           
         BAS   RE,UPDATE1F                                                      
         BNZ   SWSKIP                                                           
         BAS   RE,MAKE82EL                                                      
         BNZ   SWSKIP                                                           
         B     SW500                                                            
*                                                                               
*- SWITCH TO NEITHER  (RCONMODR+1 = X'FF'-X'C0')                                
SW300    EQU   *                                                                
         TM    RCONMODR+1,ACEMASK+TWXMASK  ALREADY NEITHER                      
         BZ    SWSKIP                                                           
*                                                                               
         NI    RCONMODR+1,X'FF'-ACEMASK-TWXMASK     MAKE IT NEITHER             
         B     SW500                                                            
         SPACE 2                                                                
*                                                                               
*- RECORD TO BE UPDATED.  INCREMENT FIXED COUNTER.                              
SW500    EQU   *                                                                
         L     RE,NUMFIX                                                        
         AH    RE,=H'1'                                                         
         ST    RE,NUMFIX                                                        
*                                                                               
SWGOOD   SR    R0,R0               NEED TO UPDATE                               
         B     TESTEXIT                                                         
SWSKIP   LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- MAKE20EL --- MAKE A CONTRACT X'20' ELEMENT (SEND ORDER)                      
*                                                                               
MAKE20EL NTR1                                                                   
*                                                                               
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFIL'),(X'20',RCONREC),(0,0), X        
               (0,0),RR=RELO                                                    
         CLI   DMCB+12,00                                                       
         BNE   MK2010                                                           
***>     GOTO1 ERROREL,DMCB,=C'20'                                              
***>     B     MK20BAD                                                          
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFIL'),(X'20',RCONREC),(0,0), X        
               (0,0),RR=RELO                                                    
MK2010   EQU   *                                                                
         XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING RCONSEND,R5                                                      
         MVI   RCONSNCO,X'20'      BUILD WITH NEW VERSION LENGTH                
         MVI   RCONSNLN,RCONSN3Q                                                
         MVC   RCONSSID(2),FAKESSID                                             
         MVI   RCONSENF,X'10'                                                   
         MVI   RCONSRV,X'01'                                                    
*        MVC   RCONSRDT(2),TODAY2BT                                             
*        UNPK  DUB,SENDTIME                                                     
*        MVC   RCONSRTI,DUB+1      TIME                                         
         MVI   RCONSSV,X'00'                                                    
*        MVC   RCONSSDT(2),TODAY2BT                                             
*        UNPK  DUB,SENDTIME                                                     
*        MVC   RCONSSTI,DUB+1      TIME                                         
         DROP  R5                                                               
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFIL'),(0,RCONREC),(0,WORK),  X        
               (0,0),RR=RELO                                                    
         CLI   DMCB+12,00                                                       
         BE    MK20GOOD                                                         
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+7,DMCB+12                                                   
         GOTO1 CANTEL,DMCB,=C'20'                                               
         BE    MK20BAD                                                          
*                                                                               
*        MAKE20EL EXIT                                                          
*                                                                               
MK20GOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
MK20BAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- UPDATE1F --- UPDATE OR ADD A X'1F' ELEMENT (EXTENDED DESCRIPTION)            
*                                                                               
UPDATE1F NTR1                                                                   
*                                                                               
UP1F10   EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFIL'),(X'1F',RCONREC),(0,0), X        
               (0,0),RR=RELO                                                    
         CLI   DMCB+12,00                                                       
         BE    UP1F20                                                           
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=X'1F18'                                                 
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFIL'),(0,RCONREC),(0,WORK),  X        
               (0,0),RR=RELO                                                    
         CLI   DMCB+12,00                                                       
         BE    UP1F10                                                           
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+7,DMCB+12                                                   
         GOTO1 CANTEL,DMCB,=C'1F'                                               
         B     UP1FBAD                                                          
UP1F20   EQU   *                                                                
         L     R5,DMCB+12                                                       
         USING RCONXEL,R5                                                       
         MVI   RCONCONF,X'40'                                                   
         LA    R6,RCONREC                                                       
         SR    R4,R4                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
UP1F30   EQU   *                                                                
         BNE   UP1F40                                                           
         ZICM  R3,6(R6),4                                                       
         AR    R4,R3                                                            
         BAS   RE,NEXTEL                                                        
         B     UP1F30                                                           
UP1F40   EQU   *                                                                
         STCM  R4,15,RCONTOT                                                    
         DROP  R5                                                               
*                                                                               
*        UPDATE1F EXIT                                                          
*                                                                               
UP1FGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
UP1FBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- MAKE82EL --- FORCE A CONTRACT X'82' ELEMENT (REP ORDER COMM)                 
*                                                                               
MAKE82EL NTR1                                                                   
*                                                                               
MK8210   EQU   *                                                                
         GOTO1 =V(HELLO),DMCB,(C'G',=C'REPFIL'),(X'82',RCONREC),(0,0), X        
               (0,0),RR=RELO                                                    
         CLI   DMCB+12,00                                                       
         BNE   MK8220                                                           
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFIL'),(X'82',RCONREC),(0,0), X        
               (0,0),RR=RELO                                                    
MK8220   EQU   *                                                                
         XC    WORK(30),WORK                                                    
         MVC   WORK(2),=X'821A'                                                 
         MVC   WORK+2(16),=C'SWITCHED BY DDS '                                  
         MVC   WORK+18(8),RCDATE                                                
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFIL'),(0,RCONREC),(0,WORK),  X        
               (0,0),RR=RELO                                                    
         CLI   DMCB+12,00                                                       
         BE    MK82GOOD                                                         
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+7,DMCB+12                                                   
         GOTO1 CANTEL,DMCB,=C'82'                                               
         B     MK82BAD                                                          
*                                                                               
*        MAKE82EL EXIT                                                          
*                                                                               
MK82GOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
MK82BAD  EQU   *                                                                
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- GETSSID --- GET REP SENDING ID FOR RCONSEND ELEM (X'20')                     
*                                                                               
GETSSID  NTR1                                                                   
*                                                                               
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     R4,MASTC                                                         
         USING MASTD,R4                                                         
         DROP  RF                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CTIREC,R6                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,MCUSERID                                                  
         DROP  R4,R6                                                            
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,AIOAREA                   
         CLI   DMCB+8,0                                                         
         BNE   SSIDBAD                                                          
         GOTO1 =V(HELLO),DMCB,(C'G',=C'CTFILE'),(X'02',IOAREA),(0,0),  X        
               (0,0),RR=RELO                                                    
         CLI   DMCB+12,00                                                       
         BNE   SSIDBAD                                                          
         L     R6,DMCB+12                                                       
         USING CTDSCD,R6                                                        
         MVC   FAKESSID,CTDSC                                                   
         DROP  R6                                                               
         B     SSIDGOOD                                                         
*                                                                               
*        GETSSID EXIT                                                           
*                                                                               
SSIDGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
SSIDBAD  EQU   *                                                                
         GOTO1 IDERR                                                            
         LA    R0,1                                                             
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*                                                                               
*        DODETAIL ---  PRINT ONE DETAIL LINE FOR A CONTRACT TO BE               
*                      UPDATED                                                  
*                                                                               
DODETAIL NTR1                                                                   
*                                                                               
         MVC   P+3(8),=C'-UPDATE-'                                              
         MVC   P+15(18),=C'CONTRACT NUMBER ='                                   
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONKCON                                                
         EDIT  (P5,DUB+3),(8,P+35)                                              
         CLI   LINE,50                                                          
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*                                                                               
*        ERROREL  ---  AN ELEMENT TO BE ADDED WAS FOUND ON THE REC              
*                                                                               
*        P1       =    A(ELEMENT CODE IN TEXT)                                  
*                                                                               
*                                                                               
*                                                                               
ERROREL  NTR1                                                                   
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
         MVC   P(43),=C'>>>XX ELEMENT FOUND ON AN OTHER CONTRACT<<<'            
         MVC   P+3(2),0(R2)                                                     
         GOTO1 REPORT                                                           
         MVC   P+3(18),=C'CONTRACT NUMBER ='                                    
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONKCON                                                
         EDIT  (P5,DUB+3),(8,P+22)                                              
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        CANTEL  ---  AN ELEMENT TO BE ADDED CANT BE                            
*                                                                               
*        P1       =    A(ELEMENT CODE IN TEXT)                                  
*        P2       =    ERROR CODE                                               
*                                                                               
*                                                                               
CANTEL   NTR1                                                                   
*                                                                               
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         MVC   P(34),=C'>>>XX ELEMENT COULD NOT BE ADDED<<<'                    
         MVC   P+3(2),0(R2)                                                     
         GOTO1 REPORT                                                           
         MVC   P+3(18),=C'CONTRACT NUMBER ='                                    
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONKCON                                                
         EDIT  (P5,DUB+3),(8,P+22)                                              
         GOTO1 REPORT                                                           
         MVC   P+3(13),=C'ERROR CODE ='                                         
         EDIT  (R3),(2,P+18)                                                    
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        IDERR  ---  ERROR GETTING REP SENDING ID                               
*                                                                               
IDERR    NTR1                                                                   
*                                                                               
         MVC   P(30),=C'>>>ERROR IN THE CONTROL FILE<<<'                        
         GOTO1 REPORT                                                           
         MVC   P(29),=C'>>>ERROR GETTING SENDING ID<<<'                         
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*- DOREPORT -- PRINT OUT RECORD COUNTERS                                        
*                                                                               
DOREPORT NTR1                                                                   
         MVC   P+1(L'#KEYS),#KEYS    # KEYS READ                                
         LA    R2,P+1+L#KEYS                                                    
         EDIT  NUMKEYS,(5,(R2)),ALIGN=LEFT,ZERO=NOBLANK                         
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+1(L'#FIX),#FIX                                                 
         LA    R2,P+1+L#FIX                                                     
         EDIT  NUMFIX,(5,(R2)),ALIGN=LEFT,ZERO=NOBLANK                          
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
         EJECT                                                                  
*                                                                               
*        COMMON ROUTINES, ET AL                                                 
*                                                                               
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         SPACE 3                                                                
         GETEL R6,34,ELCODE                                                     
         EJECT                                                                  
       ++INCLUDE RGENIO            DATA MANAGER INTERFACE                       
         EJECT                                                                  
*                                                                               
*        LITERALS                                                               
*                                                                               
#KEYS    DC    C'NUMBER OF CONTRACTS READ    '                                  
#FIX     DC    C'NUMBER OF CONTRACTS SWITCHED'                                  
*                                                                               
L#KEYS   EQU   2+L'#KEYS                                                        
L#FIX    EQU   2+L'#FIX                                                         
*                                                                               
LACE     DC    C'ACE'                                                           
LTWX     DC    C'TWX/WESTERN UNION/GRAPHNET'                                    
LOTH     DC    C'OTHER'                                                         
CONMSG   DC    C'CONTRACTS FOR STATION'                                         
SWIMSG   DC    C'WILL BE SWITCHED FROM '                                        
TO       DC    C' TO '                                                          
         DS    0H                                                               
         SPACE 3                                                                
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*        LOCAL VARIABLES AND BUFFERS                                            
*                                                                               
ACEMASK  EQU   X'80'                                                            
TWXMASK  EQU   X'40'                                                            
ACECODE  EQU   C'A'                                                             
TWXCODE  EQU   C'T'                                                             
OTHRCODE EQU   C'O'                                                             
*                                                                               
         DS    0D                                                               
         DC    C'*LOCWORK'                                                      
RELO     DS    A                                                                
*                                                                               
COUNT    DS    0F                  FULL-WORD COUNTERS                           
NUMKEYS  DS    F                   NUMBER OF KEYS READ                          
NUMFIX   DS    F                   NUMBER OF CONTRACTS SWITCHED                 
LCOUNT   EQU   *-COUNT                                                          
*                                                                               
SENDTIME DS    F                                                                
*                                                                               
ELCODE   DS    X                   ELEMENT CODE TO GETEL                        
FOOTSW   DS    CL1                 'Y' TO PRINT FOOTERS                         
SAVEMODE DS    CL1                 CURRENT CONTRACT'S RCONMODR+1                
TODAY2BT DS    CL2                 TODAY'S DATE IN 2 BYTE FORMAT                
FAKESSID DS    CL2                 THIS REP'S ID NUMBER                         
*                                                                               
         DS    0D                                                               
         DC    C'*COMMAND'                                                      
COMMAND  DS    CL8                 FOR DGENIO                                   
AIOAREA  DS    A                   A(IO BUFFER)                                 
*                                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
IOAREA   DS    2008X               IO BUFFER                                    
         EJECT                                                                  
*                                                                               
*- CONTRACT RECORD                                                              
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
*                                                                               
*  CTGENFILE                                                                    
*  DDMASTD                                                                      
*                                                                               
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
*- INFO LINE DSECT                                                              
INFOLINE DSECT                                                                  
         DS    CL1                                                              
ICONMSG  DS    CL(L'CONMSG)                                                     
         DS    CL1                                                              
ISTA     DS    CL4                                                              
IDASH    DS    CL1                                                              
IBAND    DS    CL1                                                              
         DS    CL1                                                              
ISWIMSG  DS    CL(L'SWIMSG)                                                     
         DS    CL1                                                              
ISWITO   DS    CL20                                                             
*                                                                               
*- SSB DSECT                                                                    
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018REREP1802 07/27/07'                                      
         END                                                                    

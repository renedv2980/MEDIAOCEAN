*          DATA SET NEMED69    AT LEVEL 053 AS OF 05/01/02                      
*PHASE T31E69A                                                                  
*INCLUDE NETCOM                                                                 
         TITLE 'T31E69 - NETWORK ESTIMATE WITH COMMENTS'                        
T31E69   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ESPR**,RR=R2                                                 
         ST    R2,RELO                                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2          USE W/S AREA 2 FOR W/S                       
         A     R7,=F'500'                                                       
         USING ESTD,R7                                                          
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
*                                                                               
         XC    SPOTAC(20),SPOTAC                                                
         XC    NETSTACK,NETSTACK                                                
***      L     R1,=V(DUMMY)        LOAD IN BUFFER                               
***      A     R1,RELO                                                          
         L     R1,VADUMMY          NEW SPOOF/GENCON CHANGES                     
         ST    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D908087C'                                           
         GOTO1 CALLOV,DMCB,,,0                                                  
         OC    DMCB+9(3),DMCB+9                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R1,=V(NETCOM)                                                    
         A     R1,RELO                                                          
         ST    R1,ANETCOM                                                       
         EJECT                                                                  
         MVI   NREPTYP,C'A'        SET UP AS ACCTG REPORT                       
         MVI   PERTYPE,C'M'        SET UP FOR MONTHS                            
         MVI   PERTYPE+1,1         USE MONTHS IF TOO MANY WEEKS                 
         MVI   PERTYPE+2,0         NEVER USE QUARTERS                           
*                                                                               
         MVI   RCSUBPRG,1                                                       
         CLI   MENU,0                                                           
         BNE   *+10                                                             
         MVC   MENU,NBUSER+9                                                    
         CLI   MENU,0                                                           
         BNE   *+8                                                              
         MVI   MENU,1                                                           
         CLI   MENU,12                                                          
         BNH   *+8                                                              
         MVI   MENU,1                                                           
         SPACE 2                                                                
*                                                                               
***********************                                                         
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT     PROCESS DATES                                
         BE    IN6                                                              
         B     PROCDAT             OTHER MODES ARE IGNORED                      
*                                                                               
IN6      LA    R1,MAXMONTS         SET UP MAX SIZE OF MONTH (WEEK) LIST         
         ST    R1,NUMMONS          GET LIST INTO MONLIST. NUMMONS IS            
*                                    NEW SIZE OF LIST                           
         CLI   NBDONTFD,C'Y'       MAY GET OUT OF EST M/G                       
         BNE   IN8                                                              
         GOTO1 ADDAY,DMCB,NBSELEND,WORK,90   SO FUDGE THE END DATE              
         GOTO1 DATCON,DMCB,(0,WORK),(2,NBCMPEND)                                
         SPACE 1                                                                
IN8      NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
*                                                                               
         LA    R6,MONLIST          USE BEGINNING OF MONTH LIST                  
*                                                                               
         MVI   NBDATA,C'U'         SELECT UNIT RECORDS                          
         OI    NBSPLOPT,X'80'      TURN ON PIGGY SPLIT OPTION                   
         MVI   NBSEQ,C'N'          READ IN DATE ORDER BY NETWORK                
         MVI   NBUSER+13,C'N'      DONT FILTER PRE-EMPTS                        
         MVI   NBUSER+8,C'Y'       PUT ASSIGNED COST IN CALCOST                 
         MVC   SAVENET,NBACTNET    FIRST NETWORK                                
*                                                                               
GETNEXT  NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBPROCUN     IF FIRST UNIT RECORD                         
         BE    CKNET                                                            
         CLI   NBMODE,NBREQLST     IF NO UNITS                                  
         BE    TOTALS                                                           
         B     GETNEXT             OTHER MODES ARE IGNORED                      
*                                                                               
CKNET    TM    NBSUBMSK,NBSBMNET   IF A NEW NETWORK                             
         BZ    CKDATE                                                           
         BAS   RE,MONTOTS          DO LAST MONTH TOTALS                         
         BAS   RE,NETTOTS            DO NETWORK TOTALS                          
         LA    R6,MONLIST          RESTART AT BEGINNING OF MONLIST              
         MVC   SAVENET,NBACTNET                                                 
*                                                                               
CKDATE   CLC   NBACTDAT,2(R6)      IF IN CURRENT MONTH SET                      
         BH    NEWMONTH                                                         
         NETGO NVACFLT,DMCB        PROCESS ACCOUNTING FILTER                    
         BNZ   GETNEXT             IF STATUS NZ THEN DONT PROCESS               
         BAS   RE,SPOT             FILL IN UNIT LINE.                           
         B     GETNEXT                                                          
*                                                                               
NEWMONTH BAS   RE,MONTOTS          FILL IN MONTH TOTAL                          
         LA    R6,4(R6)            POINT TO NEXT DATE-SET                       
         B     CKDATE                                                           
*                                                                               
TOTALS   BAS   RE,MONTOTS                                                       
         BAS   RE,NETTOTS                                                       
         BAS   RE,ESTTOTS                                                       
         BAS   RE,ENDCOMM          PRINT END COMMENTS                           
*                                                                               
         XMOD1                                                                  
*                                                                               
PROCERR  DC    F'0'                                                             
*                                                                               
*****************************************************************               
MONTOTS  NTR1                                                                   
         LR    R2,R6               GET CURRENT MONTH NUMBER                     
         LA    R1,MONLIST          R6 HAS A(CURRENT MONTH)                      
         SR    R2,R1                                                            
         SRL   R2,2                                                             
         MH    R2,=H'20'                                                        
         LA    R2,ESTAC(R2)                                                     
         OC    0(20,R2),0(R2)                                                   
         BZ    XIT                                                              
         GOTO1 SPOOL,DMCB,(R8)     MONTH C/B                                    
         MVC   P+6(12),=C'MONTH TOTALS'                                         
         LA    R4,P                                                             
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINES FOR A SPOT - MONTH TOTALS AND EDIT                      
*                                                                               
SPOT     NTR1                                                                   
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(4,P)                                   
         MVC   P+6(16),NBPROGNM                                                 
         MVC   P+23(3),NBDAYNAM                                                 
         GOTO1 UNTIME,DMCB,NBTIME,P+27                                          
         EDIT  (1,NBLEN),(3,P+38)                                               
         BAS   RE,PRLUP                                                         
         MVC   P+43(3),WORK                                                     
         MVC   P+43+132(3),WORK+3                                               
         B     SPOT8                                                            
         SPACE 2                                                                
PRLUP    NTR1                                                                   
         MVC   WORK,SPACES                                                      
         LA    R4,NBPRD                                                         
         CLI   NBSPLPRN,0                                                       
         BE    *+8                                                              
         LA    R4,NBSPLPRN                                                      
         LA    R5,WORK                                                          
         BAS   RE,PRLUP2                                                        
         CLI   NBSPLPRN,X'FF'                                                   
         BE    PRLUP1                                                           
         CLI   NBSPLPRN,0                                                       
         BNE   XIT                                                              
         SPACE 1                                                                
PRLUP1   LA    R4,NBPRD2                                                        
         LA    R5,WORK+3                                                        
         BAS   RE,PRLUP2                                                        
         B     XIT                                                              
         SPACE 2                                                                
PRLUP2   L     R2,ANETWS1          A(CLIENT RECORD)                             
         USING CLTHDR,R2                                                        
         LA    R2,CLIST            PRODUCT LIST                                 
         DROP  R2                                                               
         LA    R3,220                                                           
         CLI   0(R4),0                                                          
         BER   RE                                                               
         CLI   0(R4),X'FF'                                                      
         BER   RE                                                               
         SPACE 2                                                                
PRLUP4   CLC   0(1,R4),3(R2)                                                    
         BE    PRLUP6                                                           
         LA    R2,4(R2)                                                         
         BCT   R3,PRLUP4                                                        
         BR    RE                                                               
         SPACE 2                                                                
PRLUP6   MVC   0(3,R5),0(R2)                                                    
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE FOR A SPOT - DEAL WITH NUMBERS                           
         SPACE 3                                                                
SPOT8    MVI   ZEROACT,C' '        SET REAL COST SWITCHES                       
         TM    NBUNITST,X'20'                                                   
         BNO   *+8                                                              
         MVI   ZEROACT,C'0'                                                     
         MVI   ZEROASS,C' '                                                     
         TM    NBUNITST,X'08'                                                   
         BNO   *+8                                                              
         MVI   ZEROASS,C'0'                                                     
         LA    R1,1                BUILD A LINE                                 
         ST    R1,SPOTAC           (UNITS)                                      
*        L     R1,NBASSIGN                                                      
*        M     R0,=F'1'            CONVERT TO DOLLARS (FROM DOLS,CENTS)         
*        D     R0,=F'100'                                                       
*        ST    R1,SPOTAC+4                                                      
*        L     R1,NBACTUAL                                                      
*        M     R0,=F'1'            CONVERT TO DOLLARS (FROM DOLS,CENTS)         
*        D     R0,=F'100'                                                       
*        ST    R1,SPOTAC+8                                                      
         L     R0,NBASSIGN                                                      
         BAS   RE,ROUND                                                         
         ST    R1,SPOTAC+4                                                      
         L     R0,NBACTUAL                                                      
         BAS   RE,ROUND                                                         
         ST    R1,SPOTAC+8                                                      
         B     CONTIN                                                           
*                                                                               
ROUND    SR    R1,R1           CONVERT CENTS TO DOLLARS AND ROUND               
         SRDA  R0,31                                                            
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
*                                                                               
CONTIN   MVC   SPOTAC+12(4),NBINTEG                                             
         CLI   NBSELFLT,C'1'        ARE FILTERS OPERATIVE                       
         BL    SPOT10                                                           
         MVI   ZEROACT,C' '                                                     
         MVI   ZEROASS,C' '                                                     
         XC    SPOTAC+16(4),SPOTAC+16        NO CUT INS                         
*                                                                               
SPOT10   LA    R2,SPOTAC                                                        
         LA    R4,P                                                             
         OC    0(20,R2),0(R2)      IGNORE INACTIVE LINES                        
         BNZ   SPOT11                                                           
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         B     XIT                                                              
         SPACE 1                                                                
SPOT11   MVI   EPLOPT,C'Y'                                                      
         BAS   RE,FORMAT                                                        
         MVI   EPLOPT,C'N'                                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   ZEROACT,C' '                                                     
         MVI   ZEROASS,C' '                                                     
*                                                                               
         LA    R6,MONLIST          GET MONTH NUMBER IN R3                       
         SR    R3,R3                                                            
MONLOOP  CLC   NBACTDAT,2(R6)                                                   
         BNH   SPOT12                                                           
         LA    R6,4(R6)                                                         
         LA    R3,1(R3)                                                         
         B     MONLOOP                                                          
*                                                                               
SPOT12   MH    R3,=H'20'                                                        
         LA    R3,ESTAC(R3)        ADD TO MONTH                                 
         BAS   RE,ADDLIN                                                        
         LA    R3,ESTAC+320                                                     
         BAS   RE,ADDLIN           AND NETWORK TOTALS                           
         B     XIT                                                              
         SPACE 2                                                                
ADDLIN   NTR1                                                                   
         LA    R4,5                                                             
         SPACE 2                                                                
ADD2     L     R1,0(R2)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,ADD2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES AT END OF NETWORK                                       
         SPACE 3                                                                
NETTOTS  NTR1                                                                   
         LA    R2,ESTAC+320                                                     
         OC    0(20,R2),0(R2)                                                   
         BZ    XIT                                                              
         MVC   P+6(14),=C'NETWORK TOTALS'                                       
         LA    R4,P                                                             
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R3,=V(DUMMY)                                                     
         A     R3,RELO                                                          
         LA    R3,4(R3)                                                         
         LA    R4,NETSTACK                                                      
         LA    R5,15                                                            
         SPACE 2                                                                
NETTOTS2 OC    0(6,R4),0(R4)       FIND PLACE IN NETWORK LIST                   
         BZ    NETTOTS4                                                         
         LA    R3,340(R3)                                                       
         LA    R4,6(R4)                                                         
         BCT   R5,NETTOTS2                                                      
         SPACE 2                                                                
NETTOTS4 MVC   0(2,R4),SPACES                                                   
         MVC   2(4,R4),SAVENET                                                  
         BAS   RE,NETTOTS6         ADD INTO NETWORK SUMMARY BLOCK               
         LA    R3,TOTAL                                                         
         BAS   RE,NETTOTS6         AND TO ESTIMATE TOTALS                       
         MVI   FORCEHED,C'Y'                                                    
         XC    ESTAC(100),ESTAC                                                 
         XC    ESTAC+100(240),ESTAC+100                                         
         B     XIT                                                              
         SPACE 2                                                                
NETTOTS6 NTR1                      ROUTINE TO ADD BLOCKS                        
         LA    R2,ESTAC                                                         
         LA    R4,17                                                            
         SPACE 2                                                                
NETTOTS8 BAS   RE,ADDLIN                                                        
         LA    R2,20(R2)                                                        
         LA    R3,20(R3)                                                        
         BCT   R4,NETTOTS8                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL SUMMARY PRINTING                              
         SPACE 3                                                                
ESTTOTS  NTR1                                                                   
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   NETSTACK+90(12),=C'OTHERSTOTALS'                                 
         L     R2,=V(DUMMY)                                                     
         A     R2,RELO                                                          
         LA    R2,4(R2)                                                         
         LA    R3,NETSTACK                                                      
         LA    R4,16                                                            
         SPACE 2                                                                
ESTTOTS2 OC    0(100,R2),0(R2)     CONTROL THE BLOCKS                           
         BNZ   ESTTOTS4                                                         
         OC    100(240,R2),100(R2)                                              
         BZ    *+8                                                              
         SPACE 2                                                                
ESTTOTS4 BAS   RE,BLOCKAC                                                       
         LA    R2,340(R2)                                                       
         LA    R3,6(R3)                                                         
         BCT   R4,ESTTOTS2                                                      
         LA    R2,TOTAL                                                         
         BAS   RE,BLOCKAC                                                       
         B     XIT                                                              
         SPACE                                                                  
BLOCKAC  NTR1                                                                   
         MVC   P+23(6),0(R3)                                                    
         LA    R1,2                COUNT ACTIVE LINES                           
         LR    R3,R2                                                            
         LA    R4,17                                                            
         SPACE 2                                                                
BLOCK1   OC    0(20,R3),0(R3)                                                   
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R3,20(R3)                                                        
         BCT   R4,BLOCK1                                                        
         STC   R1,ALLOWLIN                                                      
         LA    R3,MONLIST                                                       
         LA    R5,16                                                            
         LA    R4,P                                                             
         SPACE 2                                                                
BLOCK2   OC    0(20,R2),0(R2)                                                   
         BZ    BLOCK4                                                           
         GOTO1 DATCON,DMCB,(2,(R3)),(9,34(R4))                                  
         CLI   NBUSER+2,C'B'       SHOW FULL DATES FOR BROADCAST                
         BNE   BLOCK3                                                           
         GOTO1 DATCON,DMCB,(2,(R3)),(4,34(R4))                                  
         MVI   39(R4),C'-'                                                      
         GOTO1 DATCON,DMCB,(2,2(R3)),(4,40(R4))                                 
         SPACE 2                                                                
BLOCK3   BAS   RE,FORMAT                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 2                                                                
BLOCK4   LA    R2,20(R2)                                                        
         LA    R3,4(R3)                                                         
         BCT   R5,BLOCK2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+34(6),=C'TOTALS'                                               
         LA    R4,P                                                             
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO FORMAT A LINE OF PRINT                                
         SPACE 3                                                                
         USING ACD,R2                                                           
FORMAT   NTR1                                                                   
         EDIT  (4,ACUN),(4,48(R4))                                              
         LA    R3,53(R4)                                                        
         MVC   SPACING,REQSPAC                                                  
         BAS   RE,SOFTCOL                                                       
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+14(20),SPLCLIN                                                
         MVC   H5+14(20),SPLPRON                                                
         MVC   H6+14(24),SPLESTN                                                
         IC    R1,NBSELFLT                                                      
         SLL   R1,28                                                            
         SRL   R1,28               0-4                                          
         MH    R1,=H'9'                                                         
         LA    R1,FILTTITL(R1)                                                  
         MVC   H6+49(9),0(R1)                                                   
         BAS   RE,COMENT           GET COMMENTS                                 
         L     R3,CURCOMM                                                       
         LA    R3,H11+53           A(HEADER)                                    
         BAS   RE,SOFTHEAD                                                      
         CLI   RCSUBPRG,2                                                       
         BE    HOOK2                                                            
         MVC   H5+75(9),=C'NETWORK -'                                           
         MVC   H5+85(4),SAVENET                                                 
         MVC   H5+90(8),SPLDPTN                                                 
         B     XIT                                                              
         SPACE 2                                                                
HOOK2    MVC   H5+75(12),=C'ALL NETWORKS'                                       
         MVC   H5+89(8),SPLDPTN                                                 
         B     XIT                                                              
         SPACE 2                                                                
XIT      XIT1                                                                   
*                                                                               
FILTTITL DC    C'(ORDERED)'                                                     
         DC    C'(CLEARED)'                                                     
         DC    C'UNCLEARED'                                                     
         DC    C' (BILLED)'                                                     
         DC    C' BILLABLE'                                                     
         EJECT                                                                  
*              ROUTINES TO EDIT COLUMNS                                         
         SPACE 3                                                                
SOFTCOL  NTR1                                                                   
         ZIC   R1,MENU             (R2=ADDRESS OF ACCUMS)                       
         BCTR  R1,0                (R3=ADDRESS OF FIRST COL)                    
         MH    R1,=H'5'                                                         
         LA    R1,MENUTAB(R1)                                                   
         LA    R0,5                                                             
         SPACE 2                                                                
SFT2     CLI   0(R1),0                                                          
         BE    *+8                                                              
         BAS   RE,SFT4                                                          
         LA    R3,12(R3)                                                        
         LA    R1,1(R1)                                                         
         BCT   R0,SFT2                                                          
         B     XIT                                                              
         SPACE 2                                                                
SFT4     NTR1                                                                   
         MVI   ZEROFLAG,C' '                                                    
         ZAP   MYDUB,=P'0'                                                      
         ZIC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     BRANCH(RF)                                                       
         SPACE 2                                                                
BRANCH   B     SFT6                1    ASS                                     
         B     SFT8                2    ASS  +INT +CUT                          
         B     SFT10               3    ACT                                     
         B     SFT12               4    ACT  +INT +CUT                          
         B     SFT10               5    GRSS                                    
         B     SFT16               6    DIFF                                    
         B     SFT18               7    INT                                     
         B     SFT20               8    CUT                                     
         B     SFT22               9    ACT  +INT                               
         B     SFT12               10   ACT  +INT +CUT                          
         B     SFT28               11   EST-PAK-LIN                             
         B     SFT24               12   COMM                                    
         B     SFT26               13   NET                                     
         B     SFT27                                                            
         SPACE 2                                                                
SFT6     L     R1,ACASS            ASS                                          
         BAS   RE,S100                                                          
         MVC   ZEROFLAG,ZEROASS                                                 
         B     SFTEND                                                           
         SPACE 1                                                                
SFT8     L     R1,ACASS            ASS  +CUT +INT                               
         BAS   RE,S100                                                          
         L     R1,ACCUT                                                         
         BAS   RE,SADD                                                          
         L     R1,ACINT                                                         
         BAS   RE,SADD                                                          
         MVC   ZEROFLAG,ZEROASS                                                 
         B     SFTEND                                                           
         SPACE 1                                                                
SFT10    L     R1,ACACT            ACT                                          
         BAS   RE,S100                                                          
         MVC   ZEROFLAG,ZEROACT                                                 
         B     SFTEND                                                           
         SPACE 1                                                                
SFT12    L     R1,ACACT            ACT  +CUT +INT                               
         BAS   RE,S100                                                          
         L     R1,ACCUT                                                         
         BAS   RE,SADD                                                          
         L     R1,ACINT                                                         
         BAS   RE,SADD                                                          
         MVC   ZEROFLAG,ZEROACT                                                 
         B     SFTEND                                                           
         SPACE 1                                                                
SFT16    L     R1,ACASS            DIFFERENCE                                   
         S     R1,ACACT                                                         
         BAS   RE,S100                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT18    L     R1,ACINT            INTEGRATION                                  
         BAS   RE,SADD                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT20    L     R1,ACCUT            CUT-IN                                       
         BAS   RE,SADD                                                          
         B     SFTEND                                                           
         SPACE 1                                                                
SFT22    L     R1,ACINT            ACT  +INT                                    
         BAS   RE,SADD                                                          
         B     SFT10                                                            
         SPACE 1                                                                
SFT24    L     R1,ACACT            COMMISSION                                   
         BAS   RE,S85                                                           
         MVC   WORK,MYDUB                                                       
         ZAP   MYDUB,=P'0'                                                      
         BAS   RE,S100                                                          
         SP    MYDUB,WORK(8)                                                    
         B     SFTEND                                                           
         SPACE 1                                                                
SFT26    L     R1,ACACT            NET                                          
         BAS   RE,S85                                                           
         MVC   ZEROFLAG,ZEROACT                                                 
         B     SFTEND                                                           
         SPACE 1                                                                
SFT27    L     R1,ACACT                                                         
         BAS   RE,S85                                                           
         L     R1,ACINT                                                         
         M     R0,=F'85'                                                        
         D     R0,=F'100'                                                       
         CVD   R1,DUB                                                           
         AP    MYDUB,DUB                                                        
         MVC   ZEROFLAG,ZEROACT                                                 
         B     SFTEND                                                           
         SPACE 1                                                                
SFTEND   EDIT  (P8,MYDUB),(12,0(R3)),2,MINUS=YES,ZERO=BLANK                     
         CP    MYDUB,=P'0'                                                      
         BNE   SFTEND2                                                          
         CLI   ZEROFLAG,C'0'                                                    
         BNE   SFTEND2                                                          
         MVC   7(4,R3),=C'0.00'                                                 
         SPACE 1                                                                
SFTEND2  B     XIT                                                              
         SPACE 1                                                                
SFT28    CLI   EPLOPT,C'Y'                                                      
         BNE   XIT                                                              
         EDIT  (1,NBACTEST),(3,1(R3))    EST-PAK                                
         EDIT  (1,NBPACK),(3,5(R3))                                             
         OC    1(7,R3),=7X'F0'                                                  
         MVI   4(R3),C'-'                                                       
         B     XIT                                                              
         SPACE 2                                                                
S100     CVD   R1,DUB                                                           
         MP    DUB,=P'100'                                                      
         AP    MYDUB,DUB                                                        
         BR    RE                                                               
         SPACE 2                                                                
SADD     CVD   R1,DUB                                                           
         AP    MYDUB,DUB                                                        
         BR    RE                                                               
         SPACE 2                                                                
S85      CVD   R1,DUB                                                           
         MP    DUB,=P'85'                                                       
         AP    MYDUB,DUB                                                        
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINES TO FILL HEADLINES                                       
         SPACE 3                                                                
SOFTHEAD NTR1                                                                   
         ZIC   R2,MENU             (R3=ADDRESS OF HEADLINE)                     
         BCTR  R2,0                                                             
         MH    R2,=H'5'                                                         
         LA    R2,MENUTAB(R2)                                                   
         LA    R4,5                                                             
         SPACE 2                                                                
SH2      ZIC   R1,0(R2)            GET DATA NUMBER                              
         LTR   R1,R1                                                            
         BZ    SH6                                                              
         BCTR  R1,0                                                             
         MH    R1,=H'12'                                                        
         LA    R1,TITLTAB(R1)      LOCATE TITLE                                 
         MVC   0(11,R3),1(R1)      MOVE IT IN                                   
         LR    R1,R3                                                            
         LA    R0,12                                                            
         SPACE 2                                                                
SH4      CLI   0(R1),C' '          UNDERLINE                                    
         BE    *+8                                                              
         MVI   132(R1),C'-'                                                     
         LA    R1,1(R1)                                                         
         BCT   R0,SH4                                                           
         SPACE 2                                                                
SH6      LA    R2,1(R2)                                                         
         LA    R3,12(R3)                                                        
         BCT   R4,SH2                                                           
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
MENUTAB  DS    0H                  COL  1    2    3    4    5                   
         DC    AL1(3,7,10,0,0)     1    ACT  INT  TOT                           
         DC    AL1(3,7,10,11,0)    2    ACT  INT  TOT  LIST                     
         DC    AL1(1,3,6,7,0)      3    ASS  ACT  DIFF INT                      
         DC    AL1(2,4,6,7,0)      4    ASS+ ACT+ DIFF INT                      
         DC    AL1(5,0,0,0,0)      5    GRSS                                    
         DC    AL1(5,13,0,0,0)     6    GRSS NET                                
         DC    AL1(5,7,09,0,0)     7    GRSS INT  TOT                           
         DC    AL1(5,7,09,14,0)    8    GRSS INT  TOT  NET                      
         DC    AL1(5,12,13,0,0)    9    GRSS COMM NET                           
         DC    AL1(5,7,9,14,11)    10   GRSS INT  TOT  NET  LIST                
         DC    AL1(0,7,0,0,0)      11        INT                                
         DC    5X'00'              12   SPARE                                   
         SPACE 2                                                                
TITLTAB  DS    0H                                                               
         DC    CL12'    ASSIGNED'  1                                            
         DC    CL12'    ASSIGNED'  2 (+INT +CUT)                                
         DC    CL12'      ACTUAL'  3                                            
         DC    CL12'      ACTUAL'  4 (+INT +CUT)                                
         DC    CL12'       GROSS'  5                                            
         DC    CL12'  DIFFERENCE'  6                                            
         DC    CL12' INTEGRATION'  7                                            
         DC    CL12'      CUT-IN'  8                                            
         DC    CL12'       TOTAL'  9  (ACT + INT)                               
         DC    CL12'       TOTAL'  10 (ACT + INT + CUT)                         
         DC    CL12' EST-PAK    '  11                                           
         DC    CL12'  COMMISSION'  12                                           
         DC    CL12'         NET'  13                                           
         DC    CL12'     NET TOT'  14                                           
         EJECT                                                                  
*              GET THE COMMENTS                                                 
         SPACE 3                                                                
COMENT   NTR1                                                                   
         LA    R4,COMMAREA                                                      
         USING NCOMBLKD,R4                                                      
         MVI   NCBTOP,C'Y'         TOP OF PAGE COMMENTS                         
         MVI   NCBKDFL,C'Y'        DEFAULT TO NEXT KEY                          
         MVI   NCBMULT,C'Y'        PASS MULTIPLE COMMENTSA                      
         MVC   NCBAIO,NBAIO                                                     
         LA    R2,NETBLOCK                                                      
         ST    R2,NCBNETB                                                       
         LA    R2,COMHOOK                                                       
         ST    R2,NCBAHOOK                                                      
         SPACE 1                                                                
         MVC   NCBAM,NBACTAM                                                    
         MVC   NCBID(2),SPLCOM        COMMENT FROM SCREEN                       
         CLI   SPLCOM+2,0             CK IF TOP COMMENTS REQUESTED              
         BE    COM1                                                             
         CLI   SPLCOM+2,C'*'                                                    
         BE    COM1                                                             
         CLI   SPLCOM+2,C'1'                                                    
         BNE   COMXIT                                                           
         SPACE 1                                                                
COM1     MVI   NCBID+2,C'1'        TOP COMMENTS                                 
         MVC   NCBCLT,NBACTCLI                                                  
         MVC   NCBPRD,NBEFFPNM                                                  
         MVC   NCBEST,NBSELEST                                                  
         MVC   NCBPKG,NBSELPAK                                                  
         MVC   NCBNTWK,SAVENET                                                  
         CLI   RCSUBPRG,2                                                       
         BNE   COM2                                                             
         MVC   NCBNTWK,NBSELNET    GET COMMENT FOR ALL NETWORKS                 
         SPACE 1                                                                
COM2     LA    R1,H7                                                            
         ST    R1,CURCOMM                                                       
         GOTO1 ANETCOM,DMCB,NCOMBLKD                                            
         MVI   NBFUNCT,NBFRESTR    RESTORE UNIT NEXT TIME IN NETIO              
         SPACE 1                                                                
COMXIT   XIT1                                                                   
         SPACE 1                                                                
COMHOOK  NTR1                                                                   
         L     R3,CURCOMM                                                       
         LA    R1,H11                                                           
         CR    R1,R3                                                            
         BE    HOOKX               NO MORE ROOM                                 
         SPACE 1                                                                
         L     R2,NBAIO                                                         
         USING NCOMELEM,R2                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   HOOKX                                                            
         SPACE 1                                                                
HOOK5    ZIC   R1,NCOMELEN                                                      
         SH    R1,=H'4'                                                         
         CH    R1,=H'35'                                                        
         BL    HOOK6               MAXIMUM COMM LENGTH = 35                     
         LA    R1,35                                                            
         SPACE 1                                                                
HOOK6    EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),NCOMETXT                                                 
         LA    R3,132(R3)                                                       
         ST    R3,CURCOMM                                                       
         LA    R1,H11                                                           
         CR    R1,R3                                                            
         BE    HOOKX               NO MORE ROOM                                 
         BAS   RE,NEXTEL                                                        
         BE    HOOK5                                                            
         SPACE 1                                                                
HOOKX    XIT1                                                                   
         EJECT                                                                  
*              PRINT END COMMENTS                                               
         SPACE 3                                                                
ENDCOMM  NTR1                                                                   
         LA    R4,COMMAREA                                                      
         USING NCOMBLKD,R4                                                      
         MVI   NCBBOT,C'Y'         LAST PAGE COMMENTS                           
         MVI   NCBKDFL,C'Y'        DEFAULT TO NEXT KEY                          
         MVI   NCBMULT,C'Y'        PASS MULTIPLE COMMENTS                       
         MVC   NCBAIO,NBAIO                                                     
         LA    R2,NETBLOCK                                                      
         ST    R2,NCBNETB                                                       
         LA    R2,ENDCOMHK                                                      
         ST    R2,NCBAHOOK                                                      
         MVC   NCBAM,NBACTAM                                                    
         MVC   NCBID(2),SPLCOM        COMMENT FROM SCREEN                       
         CLI   SPLCOM+2,0             CK IF TOP COMMENTS REQUESTED              
         BE    ECOM2                                                            
         CLI   SPLCOM+2,C'*'                                                    
         BE    ECOM2                                                            
         CLI   SPLCOM+2,C'4'                                                    
         BNE   ECOMXIT                                                          
         SPACE 1                                                                
ECOM2    MVI   NCBID+2,C'4'                                                     
         MVC   NCBCLT,NBACTCLI                                                  
         MVC   NCBPRD,NBEFFPNM                                                  
         MVC   NCBEST,NBSELEST                                                  
         MVC   NCBPKG,NBSELPAK                                                  
         MVC   NCBNTWK,NBSELNET                                                 
         GOTO1 ANETCOM,DMCB,NCOMBLKD                                            
         MVI   NBFUNCT,NBFRESTR    RESTORE UNIT NEXT TIME IN                    
         SPACE 1                                                                
ECOMXIT  XIT1                                                                   
         SPACE 1                                                                
ENDCOMHK NTR1                                                                   
         L     R3,CURCOMM                                                       
         SPACE                                                                  
         L     R2,NBAIO                                                         
         USING NCOMELEM,R2                                                      
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   EHOOKX                                                           
         SPACE 1                                                                
ECHKLOP  ZIC   R1,NCOMELEN                                                      
         SH    R1,=H'4'                                                         
         EXMVC R1,P1,NCOMETXT                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,NEXTEL           GET NEXT                                     
         BE    ECHKLOP                                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)     PRINT 2 BLANK LINES                          
         SPACE 1                                                                
EHOOKX   XIT1                                                                   
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
         GETEL (R2),NBDTADSP,ELCODE                                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR ESTIMATES                                              
         SPACE 3                                                                
ESTD     DSECT                                                                  
*** ARGS FROM EDIT                                                              
DPFILT   DS    CL1                                                              
MENU     DS    CL1                                                              
REQSPAC  DS    CL1                                                              
*                                                                               
*** LOCAL  W/S                                                                  
PERTYPE  DS    CL3                 1ST BYTE IS PERIOD TYPE                      
MAXMONTS EQU   16                  MAX MONS (WKS) IN LIST                       
MONLIST  DS    CL(4*MAXMONTS)                                                   
NUMMONS  DS    F                                                                
ANETCOM  DS    A                                                                
CURCOMM  DS    A                                                                
*                                                                               
ESTAC    DS    340C                                                             
TOTAL    DS    340C                                                             
MYDUB    DS    D                                                                
NETSTACK DS    CL102                                                            
SPOTAC   DS    5F                                                               
MASK     DS    H                                                                
DAYTWOS  DS    CL108                                                            
SAVENET  DS    CL4                                                              
EPLOPT   DS    CL1                                                              
ZEROACT  DS    CL1                                                              
ZEROASS  DS    CL1                                                              
ZEROFLAG DS    CL1                                                              
*                                                                               
COMMAREA DS    CL50                AREA FOR COMM BLOK                           
*                                                                               
SORTD    DSECT                                                                  
SORTLINE DS    CL1                 LINE NUMBER                                  
SORTDAY  DS    CL1                 DAY OF WEEK                                  
SORTWEEK DS    CL2                 ACTIVE IN WEEKS 1-16                         
*                                  REPRESENTED BY BITS 1-16                     
         SPACE 2                                                                
ACD      DSECT                                                                  
ACUN     DS    F                   UNITS                                        
ACASS    DS    F                   ASSIGNED COST                                
ACACT    DS    F                   ACTUAL COST                                  
ACINT    DS    F                   INTEGRATION                                  
ACCUT    DS    F                   CUT IN                                       
*                                                                               
*              ACCUMULATORS ARE IN 18 BLOCKS                                    
*                                                                               
*              BLOCK 1             WORK BLOCK (ESTAC)                           
*              BLOCKS 2-16         NETWORKS   (BUFFER)                          
*              BLOCK 17            OTHERS     (BUFFER)                          
*              BLOCK 18            TOTALS     (TOTAL)                           
*                                                                               
*              EACH BLOCK CONTAINS 17 LINES                                     
*                                                                               
*              LINES 1-16          MONTHS 1-16                                  
*              LINE  17            TOTAL                                        
         SPACE 2                                                                
ACWEKDOL DS    F                                                                
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDE9D                                                       
*                                                                               
         PRINT OFF                                                              
*                                  SPGENCLT                                     
*                                  NECOMBLOK                                    
*                                  NEGENCOM                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NECOMBLOK                                                      
       ++INCLUDE NEGENCOM                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053NEMED69   05/01/02'                                      
         END                                                                    

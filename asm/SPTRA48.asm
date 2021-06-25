*          DATA SET SPTRA48    AT LEVEL 076 AS OF 05/10/12                      
*PHASE T21648C                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T21648 COMMERCIAL USAGE REPORT'                                 
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - MARKET RECORDS                                             
*             AIO3 - UNUSED                                                     
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - POINTER FOR COMML TABLE                                           
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - ALSO CTR IN LR BUILD COMML TABLE RTN                   
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - UNUSED                                                            
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                               
* THIS PROGRAM READS THRU SHIIPING RECAP RECORDS AND CHECKS FOR                 
* SPOTPAK ACTIVITY                                                              
*                                                                     *         
* LEV 70    APR02/01 USE TRAFFIC OFFICE                               *         
* LEV 71 SMUR JUL09/02 CLIENT STRING SECURITY                         *         
* LEV 64 BGRI JAN15/04 CHGE SVSPARE TO TO SVSPAREX                    *         
* SEP/09 MHER CHANGES FOR ALL ADID                                    *         
* JUN/10 SMUR FIX AD-ID BUG                                           *         
* MAY/12 SMUR BUG FIX - CLOSE SORTER BETWEEN REQUESTS                 *         
*                                                                     *         
***********************************************************************         
T21648   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21648**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR48RR                                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFF-LINE ACTIVITY                            
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK10                                                             
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
*                                                                               
VK10     TM    WHEN,X'E0'          SEE IF IMMED,NOW,SOON                        
         BZ    VK12                                                             
         OC    BCLT,BCLT           THEN CLT MUST BE ENTERED                     
         BZ    CLTREQ                                                           
*                                                                               
VK12     TM    WHEN,X'10'          OVERNIGHT                                    
         BZ    VK15                 NO                                          
*                                                                               
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
*                                                                               
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
*                                                                               
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
*                                                                               
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
VK15     DS    0H                                                               
         LA    R2,TRAMKTH          MARKET                                       
         XC    BMKT,BMKT                                                        
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK20                                                             
         GOTO1 VALIMKT                                                          
*                                                                               
VK20     LA    R2,TRACMLH          COMML ID                                     
         XC    SVCMML,SVCMML                                                    
         XC    SVCMMSQ,SVCMMSQ                                                  
         BAS   RE,VCML                                                          
*                                                                               
VK30     DS    0H                                                               
         TM    WHEN,X'E0'          SEE  IF IMMED,NOW,SOON                       
         BZ    VK35                                                             
         OC    BMKT,BMKT           MARKET                                       
         BNZ   VK35                                                             
         OC    SVCMML,SVCMML       OR COMML MUST BE ENTERED                     
         BNZ   VK35                                                             
         B     MKTCOMER                                                         
*                                                                               
VK35     LA    R2,TRAPERH          PERIOD                                       
         BAS   RE,VPER                                                          
*                                                                               
         LA    R2,TRAOPTSH                                                      
         BAS   RE,VOPT            VALIDATE OPTIONS                              
*                                                                               
* NOW BUILD KEY                                                                 
*                                                                               
         XC    CMLSTIND,CMLSTIND                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'                                                  
         MVC   KEY+2(3),BAGYMD             AGY/MED AND BCLT                     
         MVC   KEY+5(2),BMKT                                                    
         MVC   KEY+10(3),SVCMMSQ           COMMERCIAL SEQ NO.                   
         MVI   FIRSTSW,0                                                        
         B     EXIT                                                             
         EJECT                                                                  
* ONLINE ACTIVITY LIST *                                                        
*                                                                               
LR       LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    TOTTOTS(12),TOTTOTS      REPORT TOTALS                           
*                                                                               
*                                   SET TO SHIPPING RECAP                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'                                                  
         MVC   KEY+2(3),BAGYMD        AND BCLT                                  
         MVC   KEY+5(2),BMKT                                                    
         MVC   KEY+10(3),SVCMMSQ      USE COMMERICAL SEQ NO                     
*                                                                               
*                                                                               
LR02     LA    R3,CMLIST                                                        
         LR    R1,R3                                                            
         SR    R1,R9                                                            
         ST    R1,CMLST                                                         
         LR    R0,R9                                                            
         A     R0,LSYSD                                                         
         SH    R0,=AL2(L'CMLENT)                                                
         ST    R0,CMLSTX                                                        
*                                                                               
         CLI   OFFLINE,C'Y'        IS THIS ONLINE                               
         BNE   LR04                                                             
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,RR=SPTR48RR                     
*                                                                               
         USING CMLISTD,R3                                                       
LR04     SR    R5,R5                                                            
         GOTO1 HIGH                                                             
         B     LR12                                                             
*                                                                               
LR10     MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
*                                                                               
LR12     MVC   SVKEY,KEY                                                        
         CLC   KEY(3),KEYSAVE     ID/AM                                         
         BNE   LR50                                                             
*                                                                               
         CLC   KEY+3(2),SVBCLT                                                  
         BNE   LR50                                                             
         OC    BMKT,BMKT           WAS MARKET ENTERED                           
         BZ    LR13                                                             
         CLC   KEY+5(2),BMKT                                                    
         BNE   LR10                                                             
*                                                                               
*                                                                               
LR13     OC    SVCMMSQ,SVCMMSQ     SEE IF COMMERCIAL ENTERED                    
         BZ    LR14                                                             
         CLC   KEY+10(3),SVCMMSQ                                                
         BNE   LR10                                                             
*                                                                               
LR14     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         LR    R4,R6                                                            
         USING SHPKEY,R4                                                        
         USING SHPDTAEL,R6                                                      
*                                                                               
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
LR20     BAS   RE,GETEL            FIND SHIP DATA ELEM                          
         BNE   LR10                                                             
*                                                                               
LR22     ST    R6,SVR6                                                          
         CLC   PERSTART,SHPSHPDT   SEE IF SHIPPED BEFORE START                  
         BH    LR40                                                             
         CLC   PEREND,SHPSHPDT     SEE IF SHIPPED AFTER END                     
         BL    LR40                                                             
         CLI   SHPPIG,0            SEE IF P/B ENTRY                             
         BNE   LR40                YES THEN SKIP THIS ELEM                      
*                                                                               
* SEARCH FOR EQUAL ENTRY OR BUILD ONE *                                         
*                                                                               
         LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   CMLMKT,SHPKMKT                                                   
         MVC   CMLFTD(3),SHPFTD     FIRST TELECAST DATE                         
         MVC   CMLLTD(3),SHPLTD     LAST TELECAST DATE                          
         MVC   CMLSTA,SHPKSTA                                                   
         MVC   CMLCML(8),SHPCMML                                                
         MVC   CMLCML2(8),SHPCMML2                                              
*                                                                               
         TM    SHPNOSHP,SHPISADI    TEST CMMLS ARE ADIDS                        
         BZ    LR24                                                             
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML),CMLCML                               
         OC    SHPCMML2,SHPCMML2                                                
         BZ    LR24                                                             
         GOTO1 (RF),(R1),(C'U',SHPCMML2),CMLCML2                                
*                                                                               
LR24     L     R3,CMLST                                                         
         AR    R3,R9                                                            
         LTR   R0,R5                                                            
         BZ    LR32                                                             
         CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BE    LR32                                                             
*                                                                               
         CLC   CMLENT,WORK                                                      
         BE    LR40                                                             
         LA    R3,CMLNEXT                                                       
         BCT   R0,*-14                                                          
         C     R3,CMLSTX                                                        
         BNL   LSTSIZER                                                         
*                                                                               
LR32     CLI   OFFLINE,C'Y'        IF OFFLINE, SORT IT                          
         BNE   LR34                                                             
         ST    RE,SVRE                                                          
         GOTO1 =V(SORTER),DMCB,=C'PUT',WORK                                     
         L     RE,SVRE                                                          
         B     LR36                BYPASS TABLE BUILD                           
*                                                                               
LR34     MVC   CMLENT,WORK                                                      
         LA    R3,CMLNEXT                                                       
         C     R3,CMLSTX                                                        
         BNL   LSTSIZER                                                         
*                                                                               
LR36     LA    R5,1(,R5)           ADD TO TABLE COUNT                           
*                                                                               
LR40     L     R6,SVR6                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,NEXTEL                                                        
         BE    LR22                                                             
         B     LR10                                                             
*                                                                               
* TABLE (IF ANY) BUILT *                                                        
*                                                                               
LR50     LTR   R5,R5               WAS ANYTHING FOUND                           
         BNZ   LR60                                                             
         CLI   OFFLINE,C'Y'                                                     
         BNE   LR50C                                                            
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
LR50C    OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    LR54                                                             
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
         GOTO1 ERREX2                                                           
*                                                                               
LR52     MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
LR54     MVC   SVBCLT,KEY+3                                                     
         MVC   KEYSAVE(2),=X'0A25'                                              
         MVC   KEYSAVE+2(1),BAGYMD                                              
         CLC   KEY(3),KEYSAVE     ID/AM                                         
         BNE   LR80                GO TO REPORT END                             
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BNZ   EXIT                                                             
         GOTO1 HIGH                                                             
         B     LR02                                                             
*                                                                               
LR60     MVC   SVKEY,KEY                                                        
         L     R3,CMLST                                                         
         AR    R3,R9                                                            
         CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BE    LR66                                                             
*                                                                               
* SORT BY COMML,MKT,STA,FTD,LTD *                                               
*                                                                               
         GOTO1 XSORT,DMCB,(R3),(R5),L'CMLENT,L'CMLENT,0                         
*                                                                               
         LR    R1,R3                                                            
         LA    R1,CMLNEXT-CMLENT(,R1)                                           
         BCT   R5,*-4                                                           
         XC    0(L'CMLENT,R1),0(R1)                                             
*                                                                               
LR66     CLI   MODE,PRINTREP       OFF-LINE ACTIVITY                            
         BE    LRR                                                              
         DC    H'0'                                                             
*                                                                               
LR80     DS    0H                                                               
         MVI   FORCEHED,C'N'                                                    
*                                                                               
         BAS   RE,RPTEND                                                        
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* OFF LINE ACTIVITY LIST *                                                      
*                                                                               
LRR      BAS   RE,FCLT                                                          
         BNE   LR52                                                             
*                                                                               
         MVI   CMLPSW,0               ZERO COMMERCIAL PRINTED SW                
         MVI   MKTPSW,0               ZERO MARKET PRINTED SW                    
         XC    RECTOTS(12),RECTOTS                                              
         XC    MKTTOTS(12),MKTTOTS                                              
         XC    CMMTOTS(12),CMMTOTS                                              
         XC    CLTTOTS(12),CLTTOTS                                              
*                                                                               
         L     R3,CMLST                                                         
         AR    R3,R9                                                            
         CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BNE   LRR10                                                            
         BAS   RE,GSRT             GO GET SORT RECORD                           
*                                                                               
********                                                                        
********            HERE I MUST READ FOR SPOT BUYS AND/OR INVOICES              
********            AND FORMAT PRINT LINE, AND ROLL TO MKT + COMML              
********            AND CLIENT, AND GRAND TOTALS                                
LRR10    MVC   PCMML,CMLCML                                                     
*                                                                               
         MVC   WORK(12),CMLCML                                                  
         XC    MYCLEN,MYCLEN      FOR PIGGYS WILL BE TOTAL OF BOTH              
*                                 COMMERCIALS                                   
         XC    MYPRDL,MYPRDL      GOTTEN ONLY FROM FIRST COMMERCIAL             
         XC    MYEPRDL,MYEPRDL    EQIV PRD LIST                                 
         XC    MYCSEQ,MYCSEQ                                                    
         XC    MYCSEQ2,MYCSEQ2                                                  
         BAS   RE,FCML                                                          
*                                                                               
         CLI   WORK+L'CMLTITLE,0  FLAG DELETED COMMERCIALS                      
         BE    *+12                                                             
         MVI   PCMML-1,C'*'                                                     
         MVI   PCMML+12,C'*'                                                    
*                                                                               
         MVC   PCMMLT,WORK                                                      
*                                                                               
         OC    CMLCML2,CMLCML2     PIGGYBACK                                    
         BZ    LRR12               NO                                           
         MVC   PCMML+132-4(3),=C'P/B'                                           
         MVC   PCMML+132,CMLCML2                                                
*                                                                               
         MVC   WORK(12),CMLCML2                                                 
         BAS   RE,FCML                                                          
*                                                                               
         CLI   WORK+L'CMLTITLE,0  FLAG DELETED COMMERCIALS                      
         BE    *+12                                                             
         MVI   PCMML-1+132,C'*'                                                 
         MVI   PCMML+12+132,C'*'                                                
*                                                                               
         MVC   PCMMLT+132,WORK                                                  
LRR12    DS    0H                                                               
         MVC   SVCOM,CMLCML          SAVE THIS COMMERCIAL                       
*                                                                               
LRR14    SR    R0,R0                                                            
         ICM   R0,3,CMLMKT                                                      
         CVD   R0,DUB                                                           
         UNPK  QMKT,DUB                                                         
         OI    QMKT+3,X'F0'                                                     
         BAS   RE,FMKT                                                          
         MVC   SVMKT,CMLMKT                                                     
LRR16    MVC   SVDTS,CMLFTD                                                     
*                                                                               
         MVC   PMKT,QMKT                                                        
         MVC   PMKTNM,MKTNM                                                     
*                                                                               
*                                                                               
LRR20    MVC   PSTA,=C'**ALL**'                                                 
         TM    SVOPTSW,X'01'           SEE IF COMBINING STATIONS                
         BO    LRR25                                                            
         MVC   DUB(2),CMLMKT           HERE IF SAME MARKET                      
         MVC   DUB+2(3),CMLSTA                                                  
         BAS   RE,FMTSTA                                                        
         MVC   PSTA,STAPRNT                                                     
*                                                                               
LRR25    L     R0,RECTOTS          BUMP SHIPPED COUNTER                         
         AH    R0,=H'1'                                                         
         ST    R0,RECTOTS                                                       
         BAS   RE,FINDBUY          SEE IF ANY BUYS FOR SHIPPED COMML            
         CLI   DUB,0                WILL BE X'01' IF I FOUND FILM               
         BE    LRR25C                                                           
         L     R0,RECTOTS+4       BUMP SCHEDULED COUNTER                        
         AH    R0,=H'1'                                                         
         ST    R0,RECTOTS+4                                                     
LRR25C   BAS   RE,FINDINV          SEE IF ANY AFFIDAVITS SHIPPED COMML          
         CLI   DUB,0                WILL BE X'01' IF I FOUND FILM               
         BE    LRR25D                                                           
         L     R0,RECTOTS+8      BUMP SHOWN COUNTER                             
         AH    R0,=H'1'                                                         
         ST    R0,RECTOTS+8                                                     
*                                                                               
LRR25D   DS    0H                                                               
         BAS   RE,RECEND        DOES SPOOL ALSO                                 
         TM    SVOPTSW,X'03'    SEE IF SUPPRESSING STA OR MKT                   
         BNZ   LRR50                                                            
         MVI   CMLPSW,1         SET ON COMMERCIAL PRINTED SW                    
         MVI   MKTPSW,1         SET ON MARKET PRINTED SW                        
         EJECT                                                                  
*                                                                               
LRR50    CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BNE   LRR52                                                            
         BAS   RE,GSRT             GO GET SORT RECORD                           
         B     LRR54                                                            
*                                                                               
LRR52    LA    R3,CMLNEXT          BUMP TABLE POINTER                           
         B     LRR54                                                            
*                                                                               
LRR54    DS    0H                                                               
         OC    CMLENT,CMLENT                                                    
         BZ    LRR60                                                            
         CLC   SVCOM,CMLCML       SEE IF SAME COMMERCIAL                        
         BNE   LRR80                                                            
         CLC   SVMKT,CMLMKT       SEE IF SAME MARKET                            
         BNE   LRR85                                                            
         B     LRR20             JUST SHOW STATION                              
*                                                                               
LRR60    BAS   RE,MKTEND                                                        
         BAS   RE,COMMEND                                                       
         BAS   RE,CLTEND                                                        
*                                                                               
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BNZ   EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   CMLPSW,0            ZERO COMML PRINTED SW                        
         MVI   MKTPSW,0            ZERO MKT PRINTED SW                          
         B     LR52                                                             
*                                                                               
LRR80    DS    0H                                                               
         BAS   RE,MKTEND                                                        
         BAS   RE,COMMEND                                                       
         XC    SVMKT,SVMKT          CLEAR MARKET                                
         MVI   CMLPSW,0             ZERO COMML PRINTED SW                       
         MVI   MKTPSW,0             ZERO MKT PRINTED SW                         
         B     LRR10                SHOW COMML,MKT,STA                          
*                                                                               
LRR85    DS    0H                   SAME COMMERCIAL - NEW MARKET                
         BAS   RE,MKTEND                                                        
         MVI   MKTPSW,0                                                         
         B     LRR14                SHOW MARKET AND STATION                     
         EJECT                                                                  
*                    FUTURE CODE WILL TRY TO FIND SPOT BUYS                     
FINDBUY  NTR                                                                    
         MVI   DUB,0                                                            
         MVI   MATCH,C'N'                                                       
         MVI   FMATCH,C'N'                                                      
         GOTO1 DATCON,DMCB,(3,CMLFTD),(2,MYFTD)                                 
         GOTO1 (RF),(R1),(3,CMLLTD),(2,MYLTD)                                   
         GOTO1 (RF),(R1),(3,CMLFTD),(0,MYFTDN)                                  
*                                  USE PRDS FROM COMMERCIAL                     
*                                  SAVED IN MYPRDL                              
         MVI   MYPRD,1       START WITH FIRST PRD IF MYPRDL IS X'FF'            
         LA    R1,MYPRDL     ELSE USE PRDS IN MYPRDL AND MYEPRDL                
         ST    R1,ANXTPRD                                                       
FINDB5   CLI   MYPRDL,X'FF'                                                     
         BE    FINDB6                                                           
         L     R1,ANXTPRD                                                       
         CLI   0(R1),0    MEANS COMML HAD NO PRDS - MAY BE IMPOSSIBLE           
         BE    FINDB55        END OF LIST                                       
*                                                                               
         MVC   MYPRD,0(R1)                                                      
FINDB6   XC    KEY,KEY                                                          
*                                                                               
         MVC   KEY(1),BAGYMD                                                    
         MVC   KEY+1(2),SVBCLT                                                  
         MVC   KEY+3(1),MYPRD                                                   
         MVC   KEY+4(5),CMLMKT       MARKET AND STATION                         
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 HIGH                                                             
         B     FINDB10                                                          
*                                                                               
FINDB8   MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 SEQ                                                              
*                                                                               
FINDB10  CLC   KEY(3),KEYSAVE                                                   
         BNE   FINDBXX               END OF CLIENT                              
         CLC   KEY(9),KEYSAVE       CHK PRD/MKT/STA                             
         BNE   FINDB50                                                          
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
         USING SPOTBUY,R6                                                       
*                                                                               
         TM    15(R6),X'80'      SEE IF DELETED                                 
         BO    FINDB8            SKIP                                           
         CLC   KEY+4(2),4(R6)    IF MKTS DIFFERENT THEN SPILL                   
         BNE   FINDB8            SKIP                                           
*                                                                               
FINDB15  L     R6,AIO2           RESET R6 TO START OF BUY                       
*                                                                               
         CLC   BDSEC,MYCLEN      TRY TO MATCH SPOT LENGTHS                      
         BNE   FINDB8                                                           
*                                                                               
*        SEE IF LINE COULD HAVE SPOT WITHIN MY TCAST DATES                      
*                                                                               
         CLC   BDEND,CMLFTD    SEE IF END BEFORE FIRST TCAST DATE               
         BL    FINDB8                                                           
         CLC   BDSTART,CMLLTD   SEE IF STARTS AFTER LAST TCAST DATE             
         BH    FINDB8                                                           
         LA    R5,BDELEM                                                        
FINDB20  BAS   RE,NXTSPOT                                                       
         BNE   FINDBX            STOP SEARCHING                                 
*                                                                               
         USING REGELEM,R5                                                       
FINDB21  CLC   RDATE,MYFTD       COMPRESSED DATES                               
         BL    FINDB20                                                          
         CLC   RDATE,MYLTD                                                      
         BH    FINDB20                                                          
         MVI   MATCH,C'Y'         FOUND MATCH ON PRD/DATE/LEN                   
*                                                                               
FINDB22  BAS   RE,TNXTEL          LOOK FOR X'12' OR X'18' ELEMS                 
         CLI   0(R5),0            END OF REC                                    
         BE    FINDBX   NO MORE ELEMS  - BUT DID MATCH PRD/DAT/LEN              
*        SEE IF THIS ELEM IS THE NEXT SPOT ELEM                                 
*        YES IF BETWEEN 06 AND 0D                                               
*                                                                               
         CLI   0(R5),X'06'                                                      
         BL    FINDB20                                                          
         CLI   0(R5),X'0D'                                                      
         BNH   FINDB21                                                          
         CLI   0(R5),X'12'        CHK IF FILM ELEM                              
         BNE   FINDB25                                                          
*                                 FIRST CHECK ELEM LENGTH                       
         CLI   1(R5),5            IF HIGHER THAN 5 IT HAS 2 FILMS               
         BH    FINDB23                                                          
         OC    MYCSEQ+2(2),MYCSEQ+2   SEE IF LOOKING FOR PIGGY BACK             
         BNZ   FINDB22              CAN'T MATCH                                 
         CLC   3(2,R5),MYCSEQ                                                   
         BE    FINDB22X                                                         
         MVI   MATCH,C'N'         WRONG FILM CODE                               
         B     FINDB22                                                          
*                                                                               
FINDB22X MVI   FMATCH,C'Y'        SET FILM MATCH                                
         B     FINDBX                                                           
*                                                                               
FINDB23  CLC   3(4,R5),MYCSEQ     BOTH MUST MATCH                               
         BE    FINDB24                                                          
         CLC   3(2,R5),MYCSEQ+2   CHK FOR REVERSED SEQUENCE                     
         BNE   FINDB20                                                          
         CLC   5(2,R5),MYCSEQ                                                   
         BNE   FINDB20                                                          
*                                                                               
FINDB24  MVI   FMATCH,C'Y'        SET FILM MATCH                                
         B     FINDBX                                                           
*                                                                               
FINDB25  CLI   0(R5),X'18'        TRAFFIC FILM ELEM                             
         BNE   FINDB22                                                          
*                                 MUST CHK ELEM LENGTH                          
         CLI   1(R5),8            SEE IF REGULAR CML ASSIGN                     
         BNE   FINDB27                                                          
         OC    2(4,R5),2(R5)       SEE IF FILMS ENTERED                         
         BZ    FINDB20             SKIP TO NEXT SPOT                            
         CLC   2(4,R5),MYCSEQ      MATCH FILM CODE(S)                           
         BE    FINDB26                                                          
         CLC   2(2,R5),MYCSEQ+2    CHK REVERSED SEQUENCE                        
         BNE   FINDB20                                                          
         CLC   4(2,R5),MYCSEQ      MATCH FILM CODE(S)                           
         BNE   FINDB20                                                          
FINDB26  MVI   FMATCH,C'Y'                                                      
         B     FINDBX              MATCH ON FILM  - DONE                        
*                                                                               
FINDB27  CLI   1(R5),9             SEE IF CML/TAG ASSIGN                        
         BNE   FINDB20             SKIP TO NEXT SPOT                            
         OC    MYCSEQ+2(2),MYCSEQ+2   SEE IF PIGGY BACK                         
         BNZ   FINDB20          THEN CAN'T MATCH-SKIP TO NEXT SPOT              
         OC    3(2,R5),3(R5)    SEE IF FILM THERE                               
         BZ    FINDB20          SKIP TO NEXT SPOT                               
         CLC   3(2,R5),MYCSEQ   ONLY MATCH 2 CHARS                              
         BNE   FINDB20          NO MATCH SKIP TO NEXT SPOT                      
         MVI   FMATCH,C'Y'                                                      
         B     FINDBX                                                           
*                                                                               
FINDB50  CLI   MYPRDL,X'FF'       SEE IF DOING ALL PRDS                         
         BE    FINDB60                                                          
         L     R1,ANXTPRD         TRY NEXT PRD FROM COMMERCIAL PRDS             
         LA    R1,1(R1)                                                         
         ST    R1,ANXTPRD                                                       
         CLI   0(R1),0              END OF LIST                                 
         BNE   FINDB5                                                           
*                                                                               
FINDB55  LA    RE,MYEPRDL           SEE IF DOING MYEPRDL                        
         CR    R1,RE                                                            
         BH    FINDBXX              DONE                                        
         LA    R1,MYEPRDL           END OF MYPRDL NOW TRY MYEPRDL               
         CLI   0(R1),0                                                          
         BE    FINDBXX              NO EQUIV PRDS                               
         ST    R1,ANXTPRD                                                       
         B     FINDB5                                                           
*                                                                               
**NOTE                                                                          
**IF MYPRDL IS X'FF', THEN FINDBUY WILL FIND ANY SPOTBUY FOR ANY                
**PRODUCT WITHIN THE SHIPPING RECAP TELECAST START-END                          
**THIS ISN'T MUCH OF A CHECK                                                    
*                                                                               
FINDB60  ZIC   R1,MYPRD                                                         
         LA    R1,1(R1)                                                         
         CH    R1,=H'255'                                                       
         BH    FINDBXX             MEANS I'VE DONE ALL PRDS                     
         STC   R1,MYPRD            TRY NEXT PRD                                 
         B     FINDB6                                                           
*                                                                               
FINDBX   TM    FILMOPT,X'01'        SEE IF FILM MATCH REQUIRED                  
         BNO   FINDBX5                                                          
         CLI   FMATCH,C'Y'                                                      
         BNE   FINDB8               MUST KEEP LOOKING                           
         B     FINDBX8                                                          
*                                                                               
FINDBX5  CLI   MATCH,C'Y'    SEE IF I FOUND PRD/DATE/LEN MATCH                  
         BNE   FINDB8        NO MUST KEEP LOOKING                               
FINDBX8  MVI   DUB,1                SET ON BUY FOUND SWITCH                     
FINDBXX  DS    0H                                                               
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R5,R6                                                            
*                                                                               
*        ROUTINE TO GET NEXT SPOT ELEM                                          
*                                                                               
NXTSPOT  ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0           END OF RECORD                                  
         BE    NXTSX                                                            
         CLI   0(R5),X'06'                                                      
         BL    NXTSPOT                                                          
         CLI   0(R5),X'0D'                                                      
         BH    NXTSPOT                                                          
         CR    RE,RE           RETURN WITH CC EQUAL                             
         BR    RE                                                               
*                                                                               
NXTSX    LTR   RE,RE           RETURN CC NOT EQUAL                              
         BR    RE                                                               
*                                                                               
TNXTEL   DS    0H                                                               
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*        THIS ROUTINE TRIES TO FIND STATION INVOICE RECORDS                     
*        THAT HAVE SPOTS ASSIGNED TO MY COMMERCIAL(S)                           
*                                                                               
FINDINV  NTR                                                                    
         MVI   DUB,0                                                            
*                                  GET PRDS FROM COMMERCIAL                     
*        NOTE THAT MYFTD AND MYLTD WERE SET IN FINDBUY                          
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(3),CMLSTA                                                  
         MVC   KEY+5(2),SVBCLT                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 HIGH                                                             
         B     FINDI10                                                          
*                                                                               
FINDI05  MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 SEQ                                                              
*                                                                               
FINDI10  CLC   KEY(7),KEYSAVE                                                   
         BNE   FINDIXX               END OF CLIENT                              
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
         CLC   KEY(9),KEYSAVE       SEE IF SAME BROADCAST MTH                   
         BE    FINDI20               YES                                        
         MVC   KEYSAVE(13),KEY                                                  
         MVI   MYINVC,0            CLEAR INV INTERNAL COMML CODES               
         MVI   MYINVC2,0                                                        
*                                                                               
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   FINDI20              GO LOOK FOR ITEMS                           
         B     FINDI17                                                          
         USING INVFLMEL,R6                                                      
FINDI15  BAS   RE,NEXTEL                                                        
         BNE   FINDI20                                                          
FINDI17  CLC   INVFCD,CMLCML        MATCH COMMERCIAL CODES                      
         BNE   FINDI18                                                          
         CLC   INVFSQ,MYCSEQ        MATCH COMMERCIAL SEQ CODES                  
         BNE   FINDI18                                                          
         MVC   MYINVC,INVFID        SAVE FILM ID                                
         B     FINDI15                                                          
*                                                                               
FINDI18  CLC   INVFCD,CMLCML2       MATCH COMMERCIAL CODES                      
         BNE   FINDI15                                                          
         CLC   INVFSQ,MYCSEQ2       MATCH COMMERCIAL SEQ CODES                  
         BNE   FINDI15                                                          
         MVC   MYINVC2,INVFID      FOR P/B COMMERCIAL                           
*                                                                               
FINDI20  DS    0H                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'B1'                                                     
         BAS   RE,GETEL                                                         
         BNE   FINDI05           NO X'B1'S GO TO SEQ                            
         B     FINDI40                                                          
*                                                                               
FINDI30  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   FINDI05           MUST BE END OF RECORD                          
         USING INVELEM,R6                                                       
FINDI40  DS    0H                                                               
*                          BECAUSE THIS IS MY CHK IF COMMERCIAL RAN             
         CLC   INVDAT,MYFTD     CHK VS. TELECAST DATES                          
         BL    FINDI30                                                          
         CLC   INVDAT,MYLTD                                                     
         BH    FINDI30                                                          
*                                                                               
         OC    INVFILM(2),INVFILM   SEE IF FILM CODES ENTERED                   
         BZ    FINDI50            NO - CAN JUST CHECK PRD AND LENGTH            
         CLI   MYINVC,0           SEE IF I HAVE FILE CODES                      
         BE    FINDI30            NO - THEN THEY CAN'T MATCH                    
*                                                                               
         CLC   INVFILM(2),MYINVC  SEE IF FILMS MATCH                            
         BE    FINDIX                                                           
         CLC   INVFILM2,MYINVC    FILM CODES MIGHT BE INVERTED                  
         BNE   FINDI30            FOR PIGGY                                     
         CLC   INVFILM,MYINVC2                                                  
         BE    FINDIX                                                           
         B     FINDI30                                                          
*                                                                               
FINDI50  TM    FILMOPT,X'01'    SEE IF FILM NOS. REQUIRED                       
         BZ    FINDI52                                                          
         B     FINDI30          YES - THEN SKIP THIS ELEM                       
*                               BECAUSE IT HAS NO FILMS                         
*                                                                               
FINDI52  CLC   INVLEN,MYCLEN    MATCH COMMERCIAL LENGTH                         
         BNE   FINDI30          SKIP THIS ELEM                                  
         CLI   MYPRDL,X'FF'     SEE IF COMMERCIAL IS FOR ALL PRDS               
         BE    FINDIX           YES - CONSIDER A MATCH                          
         LA    R1,MYPRDL                                                        
FINDI55  CLC   INVPRD,0(R1)                                                     
         BE    FINDIX                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),0           END OF LIST                                    
         BE    FINDI60           NO MATCH  - TRY EQUIV PRD LIST                 
         B     FINDI55                                                          
*                                                                               
FINDI60  LA    R1,MYEPRDL                                                       
FINDI65  CLC   INVPRD,0(R1)                                                     
         BE    FINDIX                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),0           END OF LIST                                    
         BE    FINDI30           NO MATCH  - SKIP THIS ELEM                     
         B     FINDI65                                                          
*                                                                               
FINDIX   MVI   DUB,1                                                            
FINDIXX  DS   0H                                                                
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT                               
*                                                                               
         MVC   DATADISP,=H'42'                                                  
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY          ESTABLISH NEW INVOICE RECORD KEY                 
         USING SNVKEY,R4                                                        
*                                                                               
         MVI   SNVKTYPE,SNVKTYPQ   RECORD TYPE                                  
         MVI   SNVKSUB,SNVKSUBQ    RECORD SUBTYPE                               
         MVC   SNVKAM,BAGYMD       AGENCY/MEDIA                                 
         MVC   SNVKCLT,SVBCLT      CLIENT                                       
         MVC   SNVKSTA,CMLSTA                                                   
*                                                                               
         MVC   FULL,CMLFTD                                                      
         MVI   FULL+2,01                                                        
         GOTO1 DATCON,DMCB,(3,FULL),(2,SVCMLFTD)  COMPRES'D MOS                 
         XC    SVCMLFTD,=X'FFFF'      COMPLEMENTED                              
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMLLTD),(2,SVCMLLTD)  COMPRES'D MOS               
         XC    SVCMLLTD,=X'FFFF'      COMPLEMENTED                              
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'XSPDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 HIGH                                                             
         B     FINDS10                                                          
*                                                                               
FINDS05  MVC   FILENAME,=CL8'XSPDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 SEQ                                                              
*                                                                               
FINDS10  CLC   KEY(8),KEYSAVE                                                   
         BNE   FINDSXX               END OF CLIENT                              
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'XSPFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
         CLC   KEY(9),KEYSAVE       SEE IF SAME BROADCAST MTH                   
         BE    FINDS20               YES                                        
         MVC   KEYSAVE(13),KEY                                                  
         MVI   MYINVC,0            CLEAR INV INTERNAL COMML CODES               
         MVI   MYINVC2,0                                                        
*                                                                               
         MVI   ELCODE,SNVCMELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   FINDS20              GO LOOK FOR ITEMS                           
         B     FINDS17                                                          
         USING SNVCMELD,R6                                                      
FINDS15  BAS   RE,NEXTEL                                                        
         BNE   FINDS20                                                          
FINDS17  CLC   SNVCMCD,CMLCML      MATCH COMMERCIAL CODES                       
         BNE   FINDS18                                                          
         CLC   SNVCMSEQ,MYCSEQ     MATCH COMMERCIAL SEQ CODES                   
         BNE   FINDS18                                                          
         MVC   MYINVC,SNVCMICD     SAVE INTERNAL FILM ID                        
         B     FINDS15                                                          
*                                                                               
FINDS18  CLC   SNVCMCD,CMLCML2     MATCH COMMERCIAL CODES                       
         BNE   FINDS15                                                          
         CLC   SNVCMSEQ,MYCSEQ2    MATCH COMMERCIAL SEQ CODES                   
         BNE   FINDS15                                                          
         MVC   MYINVC2,SNVCMICD    FOR P/B COMMERCIAL                           
*                                                                               
FINDS20  DS    0H                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BNE   FINDS05           NO X'40'S GO TO SEQ                            
         B     FINDS40                                                          
*                                                                               
FINDS30  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   FINDS05           MUST BE END OF RECORD                          
         USING SNVIDELD,R6                                                      
FINDS40  DS    0H                                                               
*                          BECAUSE THIS IS MY CHK IF COMMERCIAL RAN             
         ZIC   R0,SNVIDDAY                                                      
         GOTO1 ADDAY,DMCB,MYFTDN,WORK,(R0)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(2,MYTLDT)                                  
*                                                                               
         CLC   MYTLDT,MYFTD     CHK VS. TELECAST DATES                          
         BL    FINDS30                                                          
         CLC   MYTLDT,MYLTD                                                     
         BH    FINDS30                                                          
*                                                                               
         OC    SNVIDCML(2),SNVIDCML  SEE IF FILM CODES ENTERED                  
         BZ    FINDS50            NO - CAN JUST CHECK PRD AND LENGTH            
         CLI   MYINVC,0           SEE IF I HAVE FILE CODES                      
         BE    FINDS30            NO - THEN THEY CAN'T MATCH                    
*                                                                               
         CLC   SNVIDCML(2),MYINVC  SEE IF FILMS MATCH                           
         BE    FINDSX                                                           
         CLC   SNVIDCML,MYINVC    FILM CODES MIGHT BE INVERTED                  
         BNE   FINDS30            FOR PIGGY                                     
         CLC   SNVIDCM2,MYINVC2                                                 
         BE    FINDSX                                                           
         B     FINDS30                                                          
*                                                                               
FINDS50  TM    FILMOPT,X'01'    SEE IF FILM NOS. REQUIRED                       
         BZ    FINDS52                                                          
         B     FINDS30          YES - THEN SKIP THIS ELEM                       
*                               BECAUSE IT HAS NO FILMS                         
*                                                                               
FINDS52  CLC   SNVIDSLN,MYCLEN   MATCH COMMERCIAL LENGTH                        
         BNE   FINDS30            SKIP THIS ELEM                                
         CLI   MYPRDL,X'FF'      SEE IF COMMERCIAL IS FOR ALL PRDS              
         BE    FINDSX             YES - CONSIDER A MATCH                        
         LA    R1,MYPRDL                                                        
FINDS55  CLC   SNVIDPRD,0(R1)                                                   
         BE    FINDSX                                                           
         CLC   SNVIDPR2,0(R1)                                                   
         BE    FINDSX                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),0           END OF LIST                                    
         BE    FINDS60           NO MATCH  - TRY EQUIV PRD LIST                 
         B     FINDS55                                                          
*                                                                               
FINDS60  LA    R1,MYEPRDL                                                       
FINDS65  CLC   SNVIDPRD,0(R1)                                                   
         BE    FINDSX                                                           
         CLC   SNVIDPR2,0(R1)                                                   
         BE    FINDSX                                                           
         LA    R1,1(R1)                                                         
         CLI   0(R1),0           END OF LIST                                    
         BE    FINDS30           NO MATCH  - SKIP THIS ELEM                     
         B     FINDS65                                                          
*                                                                               
FINDSX   MVI   DUB,1                                                            
*                                                                               
FINDSXX  DS   0H                                                                
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO STRAFFIC                      
         MVC   DATADISP,=H'24'                                                  
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         XIT                                                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
RECEND   NTR                                                                    
         LA    R1,3                                                             
         LA    RE,MKTTOTS                                                       
         LA    RF,RECTOTS                                                       
RECE5    L     R0,0(RE)                                                         
         A     R0,0(RF)                                                         
         ST    R0,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,RECE5                                                         
         LA    R5,RECTOTS                                                       
         MVI   BYTE,0                                                           
         B     DOTOTS                                                           
*                                                                               
MKTEND   NTR                                                                    
         CLI   SVOPTSW,X'01'  SEE IF SUPPRESSING ONLY STATIONS                  
         BNE   MKTE4                                                            
         MVC   PMKTNM+132(17),=C'**MARKET TOTALS**'                             
         B     MKTE4X            SO MKTNAME WILL STILL SHOW                     
*                                                                               
MKTE4    CLI   SVOPTSW,X'03'   SEE IF SUPPRESSING STAS AND MKTS                 
         BNE   MKTE4F                                                           
         MVC   PMKTNM,SPACES                                                    
         MVC   PMKT,SPACES                                                      
         MVC   PMKTNM(17),=C'** ALL MARKETS **'                                 
         B     MKTE4X                                                           
MKTE4F   MVC   PMKTNM(17),=C'**MARKET TOTALS**'                                 
*                                                                               
MKTE4X   LA    R1,3                                                             
         LA    RE,CMMTOTS                                                       
         LA    RF,MKTTOTS                                                       
MKTE5    L     R0,0(RE)                                                         
         A     R0,0(RF)                                                         
         ST    R0,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,MKTE5                                                         
         LA    R5,MKTTOTS                                                       
         MVI   BYTE,1                                                           
         B     DOTOTS                                                           
*                                                                               
COMMEND  NTR                                                                    
         MVC   PCMMLT(16),=C'**COMML TOTALS**'                                  
         LA    RE,CLTTOTS                                                       
         LA    RF,CMMTOTS                                                       
         LA    R1,3                                                             
COME5    L     R0,0(RE)                                                         
         A     R0,0(RF)                                                         
         ST    R0,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,COME5                                                         
         MVI   BYTE,2                                                           
         LA    R5,CMMTOTS                                                       
         B     DOTOTS                                                           
*                                                                               
CLTEND   NTR                                                                    
         MVC   PCMMLT(17),=C'**CLIENT TOTALS**'                                 
         LA    RE,TOTTOTS                                                       
         LA    RF,CLTTOTS                                                       
         LA    R1,3                                                             
CLTE5    L     R0,0(RE)                                                         
         A     R0,0(RF)                                                         
         ST    R0,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,CLTE5                                                         
         LA    R5,CLTTOTS                                                       
         MVI   BYTE,2                                                           
         MVI   CMLPSW,0                                                         
         MVI   MKTPSW,0                                                         
         B     DOTOTS                                                           
*                                                                               
RPTEND   NTR                                                                    
         MVC   PCMMLT(17),=C'**REPORT TOTALS**'                                 
         LA    R5,TOTTOTS                                                       
         MVI   BYTE,2                                                           
         MVI   CMLPSW,0                                                         
         MVI   MKTPSW,0                                                         
         B     DOTOTS                                                           
*                                                                               
DOTOTS   DS    0H                                                               
         EDIT  (B4,0(R5)),(7,PSHIP),0,COMMAS=YES                                
         EDIT  (B4,4(R5)),(9,PSCHD),0,COMMAS=YES                                
         EDIT  (B4,8(R5)),(6,PSHOWN-1),0,COMMAS=YES                             
         L     R0,0(R5)                                                         
         S     R0,4(R5)                                                         
         EDIT  (R0),(6,PEXCS),0,COMMAS=YES                                      
         CLI   BYTE,0                  RECTOTS                                  
         BNE   DOTOT5                                                           
*                         FOR RECTOTS SHOW 'Y' AND 'N' NOT 1 AND 0              
         MVI   PSHIP+6,C'Y'           WILL ALWAYS BE YES                        
         MVI   PSCHD+8,C'N'                                                     
         OC    4(4,R5),4(R5)                                                    
         BZ    *+8                                                              
         MVI   PSCHD+8,C'Y'                                                     
         MVI   PSHOWN+4,C'N'                                                    
         OC    8(4,R5),8(R5)                                                    
         BE    *+8                                                              
         MVI   PSHOWN+4,C'Y'                                                    
         XC    0(12,R5),0(R5)         CLEAR ACCUMS AFTER PRINTING               
         B     DOTOTXX                                                          
*                                                                               
DOTOT5   DS    0H                                                               
         XC    0(12,R5),0(R5)         CLEAR ACCUMS AFTER PRINTING               
         OI    PSHIP+L'PSHIP-1,C'0'                                             
         OI    PSCHD+L'PSCHD-1,C'0'                                             
         OI    PSHOWN+L'PSHOWN-1,C'0'                                           
         OI    PEXCS+L'PEXCS-1,C'0'                                             
         MVI   PSHIP+L'PSHIP,C'*'                                               
         MVI   PSCHD+L'PSCHD,C'*'                                               
         MVI   PSHOWN+L'PSHOWN,C'*'                                             
         MVI   PEXCS+L'PEXCS,C'*'                                               
         CLI   BYTE,1                                                           
         BE    DOTOTX                                                           
*                                   MUST BE TWO STARS                           
         MVI   PSHIP+L'PSHIP+1,C'*'                                             
         MVI   PSCHD+L'PSCHD+1,C'*'                                             
         MVI   PSHOWN+L'PSHOWN+1,C'*'                                           
         MVI   PEXCS+L'PEXCS+1,C'*'                                             
DOTOTX   MVI   SPACING,2                                                        
DOTOTXX  CLI   CMLPSW,1            SEE IF COMMERCIAL ALREADY PRINTED            
         BNE   DOTOTXXX                                                         
         ZIC   R0,LINE                                                          
         ZIC   R1,SPACING                                                       
         AR    R0,R1                                                            
         STC   R0,DUB                                                           
         CLC   DUB,MAXLINES       CK LINE VS. MAXLINE                           
         BL    DOTOTXXX                                                         
*                            NEW PAGE  - SO PRINT CONTINUED MESSAGE             
         MVC   PCMML,SVCOM                                                      
         OC    SVCOM+12(12),SVCOM+12                                            
         BZ    DOTOTXX5                                                         
         MVC   PCMML+132-4(3),=C'P/B'                                           
         MVC   PCMML+132(12),SVCOM+12                                           
DOTOTXX5 CLC   PCMMLT(5),=C'**COM'    SEE ID DOING COMMERCIAL TOTALS            
         BE    DOTOTXXX                                                         
         MVC   PCMMLT(11),=C'(CONTINUED)'                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SVMKT                                                       
         CVD   R0,DUB                                                           
         UNPK  PMKT,DUB                                                         
         OI    PMKT+3,X'F0'                                                     
         CLC   PMKTNM(5),=C'**MAR'        SEE IF DOING MKT TOTALS               
         BE    DOTOTXXX                                                         
         CLI   MKTPSW,0                    SEE IF MARKET PRINTED                
         BE    *+10                        NO - LEAVE NAME THERE                
         MVC   PMKTNM(11),=C'(CONTINUED)'                                       
*                                                                               
DOTOTXXX DS    0H                                                               
         CLI   BYTE,0             SEE IF DOING STATION LINE                     
         BNE   DOTOTXXY                                                         
         TM    SVOPTSW,X'01'      SEE IF COMBINING STATIONS                     
         BO    DOTOTXX9                                                         
         B     DOTOTXXZ                                                         
*                                                                               
DOTOTXXY CLI   BYTE,1            SEE IF DOING MARKET LINE                       
         BNE   DOTOTXXZ                                                         
         TM    SVOPTSW,X'02'     SEE IF COMBINING MKTS                          
         BO    DOTOTXX9                                                         
*                                                                               
DOTOTXXZ GOTO1 SPOOL,DMCB,(R8)                                                  
DOTOTXX9 XIT                                                                    
         EJECT                                                                  
* VALIDATE COMMERCIAL RECORD *                                                  
*                                                                               
VCML     NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    EXIT                                                             
         GOTO1 ANY                                                              
         CLI   5(R2),8                                                          
         BL    CMLENER                                                          
         CLI   5(R2),12                                                         
         BH    CMLENER                                                          
*                                                                               
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(8),WORK                                                    
         CLI   5(R2),8                                                          
         BE    VCML2                                                            
*                                                                               
         MVC   KEY(2),=X'0AC1'     SET TO READ FOR ADID                         
         GOTO1 VTRPACK,DMCB,(C'P',WORK),KEY+5                                   
         BNE   BADCOMM                                                          
*                                                                               
VCML2    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADCOMM                                                          
*                                                                               
VCML10   MVC   SVCMML,WORK                                                      
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'       FIND SEQNUM                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'               BAD RECORD                                    
         MVC   SVCMMSQ,2(R6)                                                    
*                                                                               
VCMLX    B     EXIT                                                             
*                                                                               
* VALIDATE PERIOD                                                               
*                                                                               
VPER     NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         XC    PEREND,PEREND                                                    
         LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),WORK                                            
         L     R5,DMCB             GET LENGTH OF FIELD                          
         LTR   R5,R5                                                            
         BZ    DATERR                                                           
         LA    R3,1(R5,R3)         POINT TO END DATE                            
         GOTO1 DATCON,(R1),(0,WORK),(3,PERSTART)                                
         CLM   R5,1,5(R2)          WAS ONLY 1 DATE ENTERED                      
         BE    EXIT                YES, ALL DONE                                
VPER10   GOTO1 DATVAL,DMCB,(R3),WORK                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,PEREND)                                  
         CLC   PERSTART,PEREND                                                  
         BH    DATERR                                                           
         B     EXIT                                                             
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
* 1                                                                             
* VALID OPTIONS ARE  'DETAIL'    OPTDETL  (X80) PRINT DETAIL STATIONS           
*                                                                               
VOPT     NTR1                                                                   
*                                                                               
         MVI   SVOPTSW,0                                                        
         MVI   FILMOPT,0                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPT96              NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPTHLP             YES                                          
         CLI   5(R2),4                                                          
         BNH   VOPT02                                                           
         LA    R1,3                                                             
         B     VOPT04                                                           
VOPT02   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
VOPT04   EX    R1,VOPTCLCJ         HELP                                         
         BE    VOPTHLP                                                          
*                                                                               
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,TRAOPTSH,(7,(R4))                                   
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
VOPT10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,VOPTCLCA         STATION DETAIL  (NOSTA)                      
         BNE   VOPT20                                                           
         OI    SVOPTSW,X'01'       SUPPRESS STATION DETAILS                     
         B     VOPT90                                                           
*                                                                               
VOPT20   EX    R1,VOPTCLCB         MARKET DETAIL  (NOMKT)                       
         BNE   VOPT30                                                           
         OI    SVOPTSW,X'03'       SUPPRESS STA AND MKT DETAILS                 
         B     VOPT90                                                           
*                                                                               
VOPT30   EX    R1,VOPTCLCC         MARKET DETAIL  (NOMAR)                       
         BNE   VOPT40                                                           
         OI    SVOPTSW,X'03'       SUPPRESS STA AND MKT DETAILS                 
         B     VOPT90                                                           
*                                                                               
VOPT40   EX    R1,VOPTCLCD         FILM DETAIL                                  
         BNE   VOPTHLP                                                          
         OI    FILMOPT,1           MEANS MUST USE FILM NUMBERS                  
*                           WHEN LOOKING FOR BUYS AND INVS                      
*                                                                               
VOPT90   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
VOPT96   B     EXIT                                                             
*                                                                               
VOPTHLP  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(23),=CL23'*OPTION IS NOSTA,NOMKT*'                       
         B     ERREXIT                                                          
*                                                                               
VOPTCLCA CLC   12(0,R4),=CL5'NOSTA'                                             
VOPTCLCB CLC   12(0,R4),=CL5'NOMKT'                                             
VOPTCLCC CLC   12(0,R4),=CL5'NOMAR'      SAME AS NO MKT                         
VOPTCLCD CLC   12(0,R4),=CL5'FILM'                                              
VOPTCLCJ CLC   8(0,R2),=CL4'HELP'                                               
         EJECT                                                                  
* GET SORT RECORDS, ELIMINATING EQUALS *                                        
*                                                                               
GSRT     NTR1                                                                   
*                                                                               
GSRT10   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     RF,4(R1)                                                         
         LTR   RF,RF               CK EOF                                       
         BZ    GSRT20               YES                                         
*                                                                               
         MVI   FIRSTSW,1                                                        
         CLC   CMLENT,0(RF)        EQUAL REC                                    
         BE    GSRT10                                                           
         MVC   CMLENT,0(RF)        MOVE FOR VISIBLITY                           
         CR    R0,R0                                                            
         B     EXIT                                                             
*                                                                               
GSRT20   XC    CMLENT,CMLENT       SET FOR EOF                                  
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLI   FIRSTSW,0           TEST FIRST TIME                              
         BNE   EXIT                NO                                           
         MVC   P(16),=C'NO INPUT RECORDS'                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         ABEND 999                                                              
         EJECT                                                                  
* READ MARKET RECORD *                                                          
*                                                                               
FMKT     NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
*                                                                               
         MVC   MKTNM,=CL24'**** UNKNOWN ****'                                   
         L     R1,AIO                                                           
         CLC   KEY(8),0(R1)                                                     
         BNE   EXIT                                                             
*                                                                               
         USING MKTRECD,R1                                                       
         MVC   MKTNM,MKTNAME                                                    
         B     EXIT                                                             
         DROP  R1                                                               
FMTSTA   NTR1                                                                   
*                                                                               
         GOTO1 MSUNPK,DMCB,DUB,FULL,WORK                                        
         CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
* 1                                                                             
* FORMAT STATION FOR PRINTING *                                                 
* 1                                                                             
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),WORK                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),WORK+4                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    EXIT                                                             
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    EXIT                                                             
         MVI   3(RE),C' '                                                       
         B     EXIT                                                             
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
*                                                                               
FCLT     NTR1                                                                   
*                                                                               
         MVC   BBCLT,BCLT          SAVE                                         
*                                                                               
FCLT10   GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         MVC   SVKEY,KEY                                                        
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
*                                                                               
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEY,SVKEY                                                        
         MVI   KEY+5,X'FF'         GET NEXT CLIENT                              
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      IF SAME REC TYPE & A/M                       
         BNE   FCLTNE                                                           
         MVC   SVBCLT,KEY+3        SAVE CLIENT                                  
         B     FCLT10                                                           
*                                                                               
FCLT20   MVC   BCLT,BBCLT          RESTORE                                      
         CR    RB,RB                                                            
         B     FCLTX                                                            
*                                                                               
FCLTNE   LTR   RB,RB                                                            
FCLTX    XIT1                                                                   
*                                                                               
         B     EXIT                                                             
*                                                                               
* FIND COMMERCIAL TITLE *                                                       
*                                                                               
FCML     NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,WORK                                                     
*                                                                               
         OC    WORK,SPACES                                                      
*                                                                               
         CLI   WORK+8,C' '         TEST FOR ADID                                
         BE    FCML2                                                            
*                                                                               
         MVC   CMLKID,=X'0AC1'                                                  
         GOTO1 VTRPACK,DMCB,(C'P',WORK),CMLKCML                                 
*                                                                               
FCML2    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FCML4                                                            
         MVC   WORK(15),=C'CAN''T FIND CMML'                                    
         B     FCMLX                                                            
*                                                                               
FCML4    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         MVC   WORK(L'CMLTITLE),CMLTITLE                                        
         MVI   WORK+L'CMLTITLE,0                                                
         TM    CMLSTAT,X'80'                                                    
         BZ    *+8                                                              
         MVI   WORK+L'CMLTITLE,C'*'                                             
*                                                                               
         ZIC   R0,MYCLEN                                                        
         ZIC   RE,CMLSLN                                                        
         AR    R0,RE                                                            
         STC   R0,MYCLEN              SAVE TOTAL LENGTH                         
*                                                                               
         LA    RE,MYCSEQ                                                        
         OC    MYCSEQ,MYCSEQ                                                    
         BZ    *+8                                                              
         LA    RE,MYCSEQ2                                                       
         MVC   0(2,RE),CMLSEQ+1       SAVE SEQ NUMBER (LAST 2 BYTES)            
*                                     SPOTBUYS AND STATION INVS ONLY            
*                                     STORE LAST 2 BYTES                        
*                                                                               
         OC    MYPRDL,MYPRDL   SEE IF I ALREADY HAVE PRODUCT  LIST              
         BNZ   FCMLX                                                            
         L     R6,AIO1                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   FCMLX                                                            
         USING CMLPRDEL,R6                                                      
         ZIC   R1,CMLPRDLN                                                      
         SH    R1,=H'3'                                                         
         BM    FCMLX                                                            
         EX    R1,FCML10                                                        
         B     *+10                                                             
*                                                                               
FCML10   MVC   MYPRDL(0),CMLPRDS                                                
*                                                                               
         CLI   MYPRDL,X'FF'        SEE IF ALL PRDS                              
         BE    FCMLX               SKIP EQUIV PRD READ                          
*                                                                               
*        NOW CHK FOR EQUIVALENT PRDS AND ADD THEM TO LIST                       
         LA    R6,MYPRDL                                                        
         ST    R6,ANXTPRD                                                       
*                                                                               
FCML12   DS    0H                                                               
         L     R6,ANXTPRD                                                       
         CLI   0(R6),0                                                          
         BE    FCMLX          DONE                                              
*                                                                               
         L     R3,ASVCLIST                                                      
         LA    R4,220                                                           
FCML15   CLI   3(R3),0       END OF CLIST                                       
         BE    FCMLERR       PRD NOT IN SVCLIST                                 
         CLC   3(1,R3),0(R6)                                                    
         BE    FCML20                                                           
         LA    R3,4(R3)                                                         
         BCT   R4,FCML15                                                        
         B     FCMLERR       PRD NOT IN CLIST                                   
*                                                                               
FCML20   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A37'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),SVBCLT                                                  
         MVC   KEY+5(3),0(R3)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   FCML50        GO GET NEXT PRD IN MYPRDL                          
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   FCML50                                                           
         B     FCML26                                                           
*                                                                               
FCML25   BAS   RE,NEXTEL                                                        
         BNE   FCML50                                                           
*                                                                               
FCML26   L     R5,ASVCLIST                                                      
         LA    R1,220                                                           
FCML28   OC    0(3,R5),0(R5)         SEE IF AT END OF LIST                      
         BZ    FCML25         EQUIV PRD NOT FOUND  - JUST SKIP IT               
         CLC   0(3,R5),2(R6)         FIND EQIV PRD CODE                         
         BE    FCML30                                                           
         LA    R5,4(R5)                                                         
         BCT   R1,FCML28                                                        
         B     FCML25               PRD NOT IN SVCLIST                          
*                                                                               
FCML30   LA    R1,MYEPRDL                                                       
FCML31   CLI   0(R1),0                                                          
         BE    FCML35                                                           
         CLC   0(1,R1),3(R5)        SEE IF ALREADY IN LIST                      
         BE    FCML25               YES - THEN SKIP                             
         LA    R1,1(R1)                                                         
         B     FCML31                                                           
*                                                                               
FCML35   MVC   0(1,R1),3(R5)        SAVE EQIV PRD BINARY CODE                   
         B     FCML25               GO DO NEXT ELEM                             
*                                                                               
FCML50   L     R6,ANXTPRD           DO NEXT PRD IN MYPRDL                       
         LA    R6,1(R6)                                                         
         ST    R6,ANXTPRD                                                       
         B     FCML12                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
FCMLX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
FCMLERR  DC    H'0'                 BAD PRODUCT CODE - NOT IN SVCLIST           
*                                                                               
         EJECT                                                                  
*                                                                               
HDHK     NTR1                                                                   
         MVC   H2+10(1),QMED                                                    
         MVC   H2+15(L'MEDNM),MEDNM                                             
         MVC   H3+10(L'QCLT),QCLT                                               
         MVC   H3+15(L'CLTNM),CLTNM                                             
         MVC   H3+36(8),=C'PERIOD ='                                            
         GOTO1 DATCON,DMCB,(3,PERSTART),(5,H3+45)                               
         MVI   H3+53,C'-'                                                       
         GOTO1 (RF),(R1),(3,PEREND),(5,H3+54)                                   
         B     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
LSTSIZER TM    WHEN,X'40'          THIS NOW                                     
         BZ    LSTSIZPT            NO PRINT MESSAGE                             
         MVC   CONHEAD,LSTSIZMS                                                 
         B     ERREXIT                                                          
LSTSIZPT XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         MVC   P+20(33),=CL33'REQUEST REPORT FOR SMALLER PERIOD'                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
CLTREQ   MVC   CONHEAD,CLTREQMS                                                 
         B     ERREXIT                                                          
MKTCOMER MVC   CONHEAD,MKTCOMMS                                                 
         B     ERREXIT                                                          
OLER     MVC   CONHEAD,OLERMS                                                   
ERREXIT  GOTO1 ERREX2                                                           
*                                                                               
CMLENER  MVC   GERROR,=Y(NOT812)                                                
         GOTO1 VTRAERR                                                          
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
BADCOMM  MVI   ERROR,INVCOMM                                                    
         B     TRAPERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
LSTSIZMS DC    CL60'* ERROR * LIST TOO LARGE FOR ONLINE, RUN SMALLER ORC        
                OV *'                                                           
CLTREQMS DC    CL60'* ERROR * CLIENT REQUIRED FOR ONLINE LIST *'                
MKTCOMMS DC    CL60'* ERROR * MKT OR COMML REQUIRED FOR NOW,SOON *'             
OLERMS   DC    CL60'* ERROR * ONLINE LIST NOT SUPPORTED *'                      
SORTCARD DC    CL80'SORT FIELDS=(1,35,A),FORMAT=BI '                            
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=35 '                                   
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,39,C'COMMERCIAL USAGE REPORT'                                 
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H2,39,C'-----------------------'                                 
         SSPEC H2,73,AGYADD                                                     
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H3,85,RUN                                                        
         SSPEC H3,73,REPORT                                                     
         SSPEC H4,73,REQUESTOR                                                  
         SSPEC H4,103,PAGE                                                      
         SSPEC H8,5,C'COMMERCIAL      NAME'                                     
         SSPEC H9,5,C'------------    ----'                                     
         SSPEC H8,40,C'MARKET NAME                     STATION'                 
         SSPEC H9,40,C'------ -----                    -------'                 
         SSPEC H8,84,C'SHIPPED  SCHEDULED  EXCESS  SHOWN'                       
         SSPEC H9,84,C'-------  ---------  ------  -----'                       
         DC    X'00'               END MARKER FOR SSPECS                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRSHIP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPRDEQV                                                     
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
SPOTCLT  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
SPOTBUY  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
SPOTINV  DSECT                                                                  
       ++INCLUDE SPGENINV                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
* INCLUDED DSECTS                                                               
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
*                                                                               
         DS    CL4                                                              
PCMML    DS    CL12                                                             
         DS    CL4                                                              
PCMMLT   DS    CL15                                                             
         DS    CL4                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PMKTNM   DS    CL24                                                             
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PSHIP    DS    CL7                                                              
         DS    CL2                                                              
PSCHD    DS    CL9                                                              
         DS    CL2                                                              
PEXCS    DS    CL6                                                              
         DS    CL2                                                              
PSHOWN   DS    CL5                                                              
         DS    CL2                                                              
* INCLUDED DSECTS                                                               
* INCLUDE DDCOMFACS                                                             
* INCLUDE FAFACTS                                                               
* INCLUDE SPTRAWORKD                                                            
*        PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAF0D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
GEND     DSECT                                                                  
         ORG   ELEM                                                             
ASVSTOR  DS    A                                                                
VTRPACK  DS    A                                                                
SVR1R2   DS   2F                                                                
SVR6     DS    F                                                                
SVRE     DS    F                                                                
*                                                                               
* KEEP CMLPTR AND STACT TOGETHER AND IN ORDER                                   
*                                                                               
CMLCT    DS    F                                                                
CMLPTR   DS    F                                                                
STACT    DS    F                                                                
*                                                                               
CMLST    DS    F                                                                
CMLSTX   DS    F                                                                
ANXTPRD  DS    F                                                                
*                                                                               
MYCLEN   DS    XL1                COMML LENGTH                                  
*                                (WILL BE SUM FOR P/B COMMERCIALS)              
MYCSEQ   DS    XL2                SEQ NO.                                       
MYCSEQ2  DS    XL2                P/B SEQ NO.                                   
MYINVC   DS    XL1                INV INTERNAL COMML CODE                       
MYINVC2  DS    XL1                INV INTERNAL COMML CODE                       
MYPRD    DS    XL1                CURRENT PRD IN FINDBUY                        
MYFTD    DS    XL2                CMLFTD - PACKED                               
MYLTD    DS    XL2                CMLLTD - PACKED                               
MYFTDN   DS    CL6                CMLFTD - ZD                                   
MYTLDT   DS    CL2                                                              
*                                                                               
SVMKT    DS    XL2                                                              
SVCOM    DS    XL24              LAST COMMERCIAL FROM TABLE                     
SVDTS    DS    XL6                                                              
SVCMML   DS    CL8                ENTER COMMERCIAL                              
SVCMMSQ  DS    XL3                CMML SEQ NUMBER                               
SVOPTSW  DS    XL1                X'01' = SUPPRESS STATIONS                     
*                                 X'03' = SUPPRESS MARKETS AND STATIONS         
FILMOPT  DS    XL01               X'01' = REQUIRE FILM NUMBERS WHEN             
*                                         LOOKING FOR BUYS AND INVS             
CMLPSW   DS    XL1                X'01' IF COMMERCIAL PRINTED                   
MKTPSW   DS    XL1                X'01' IF MARKET PRINTED                       
OPTDETL  EQU   X'80'              PRINT ALL STATIONS                            
FIRSTSW  DS    XL1                                                              
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR48RR DS    F                                                                
CMLSTIND DS    F                                                                
RECTOTS  DS    3F            SHIPPED/SCHEDULED/SHOWN                            
MKTTOTS  DS    3F            SHIPPED/SCHEDULED/SHOWN                            
CMMTOTS  DS    3F                                                               
CLTTOTS  DS    3F                                                               
TOTTOTS  DS    3F                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
BBCLT    DS    CL2                 SAVE BCLT                                    
PERSTART DS    XL3                                                              
PEREND   DS    XL3                                                              
SVBCLT   DS    XL2                                                              
SVCMLFTD DS    XL2                                                              
SVCMLLTD DS    XL2                                                              
MATCH    DS    CL1                                                              
FMATCH   DS    CL1                                                              
*                                                                               
MYPRDL   DS    XL30               LIST OF PRDS FROM COMMERCIAL                  
*                                                                               
MYEPRDL  DS    XL220           LIST OF EQUIV PRDS FOR COMML PRDS                
*                                                                               
* LIST OF MARKETS/STAS BY COMMERCIAL                                            
* BUILT HERE FOR NOW, OR AT DUMMY OFFLINE                                       
*                                                                               
CMLIST   DS    CL1                                                              
*                                                                               
* CML LIST *                                                                    
*                                                                               
CMLISTD  DSECT                                                                  
CMLENT   DS    0XL35                                                            
CMLCML   DS    CL12                                                             
CMLCML2  DS    CL12                                                             
CMLMKT   DS    XL2                                                              
CMLSTA   DS    XL3                                                              
CMLFTD   DS    XL3                                                              
CMLLTD   DS    XL3                                                              
CMLNEXT  EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076SPTRA48   05/10/12'                                      
         END                                                                    

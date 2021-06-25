*          DATA SET SPSFM45    AT LEVEL 007 AS OF 04/26/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE T21745A                                                                  
*====================================================================*          
* TYPE   CODE                                                        *          
*  A2     A                                                          *          
*  BC     B                                                          *          
*  ACOM   1                                                          *          
*  A3     3                                                          *          
*  B4     4                                                          *          
*  B5     5                                                          *          
*  B6     6                                                          *          
*  B7     7                                                          *          
*  SCOM   T                                                          *          
*  BC     B                                                          *          
*  MCOM   M                                                          *          
*  RSCOM  R                                                          *          
*  NVTEXT N                                                          *          
*  SDR    S                                                          *          
*  I2     I                        LIVES ON XSPFIL                   *          
*  PCOM   P                                                          *          
*====================================================================*          
T21745   TITLE 'SPSFM45 - COMMENTS'                                             
T21745   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21745,R8                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         BAS   RE,INIT                                                          
*                                                                               
         CLI   MODE,SETFILE        SETFILE FOR I2COM (XFILE)                    
         BE    SF                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,XRECADD        ADDED RECORD                                 
         BE    REQ                                                              
         CLI   MODE,XRECPUT        CHANGED RECORD                               
         BE    REQ                                                              
         CLI   MODE,XRECDEL        DELETED RECORD                               
         BE    REQ                                                              
         CLI   MODE,XRECREST       RESTORED RECORD                              
         BE    REQ                                                              
         B     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        INITIALIZATIONS                                                        
*                                                                               
INIT     NTR1                                                                   
         OI    CONSERVH+6,X'81'    FORCE SCREEN CHANGE FOR PFKEYS               
*                                                                               
INIT10   CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   *+12                                                             
         OI    SCMLN1H+6,X'81'     FORCE SCREEN CHANGE FOR PFKEYS               
         B     INIT20                                                           
         CLI   CONREC,C'I'         I2COM RECS?                                  
         BNE   *+12                                                             
         OI    SI2LN1H+6,X'81'                                                  
         B     *+8                                                              
         OI    SCOLN1H+6,X'81'     FORCE SCREEN CHANGE FOR PFKEYS               
*                                                                               
INIT20   CLI   PFKEY,0                                                          
         BE    EXIT                                                             
         OI    GENSTAT2,RETEQSEL                                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* VALIDATE KEY                                                                  
*                                                                               
VK       CLC   SVRECNUM,TWALREC    IF RECORD TYPE CHANGED                       
         BE    VK05                                                             
         NI    SCOMEDH+4,X'DF'     FORCE VALIDATION OF ALL FLDS                 
*                                                                               
         XC    BCLT,BCLT                                                        
         XC    MYPRD,MYPRD         AND CLEAR INPUT                              
         MVI   BEST,0                                                           
         XC    BSTA,BSTA                                                        
*                                  CLR NAME FIELDS                              
*                                                                               
***************                                                                 
* MEDIA FIELD                                                                   
***************                                                                 
VK05     LA    R2,SCOMEDH          MEDIA                                        
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SCOCLTH+4,X'DF'     FORCE VALIDATION OF NEXT FLD                 
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         CLI   CONREC,C'N'         NVTEXT - MEDIA 'C' INVALID                   
         BNE   *+12                                                             
         CLI   QMED,C'C'                                                        
         BE    INVERR                                                           
         OI    4(R2),X'20'                                                      
***************                                                                 
* CLIENT FIELD                                                                  
***************                                                                 
VK10     LA    R2,SCOCLTH          CLIENT FIELD                                 
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         NI    SCOPRDH+4,X'DF'     FORCE VALIDATION OF NEXT FLD                 
         XC    BCLT,BCLT                                                        
         CLI   CONREC,C'M'         MCOM, SCOM, SDR COMMENTS NEED CLT            
         BE    VK11                                                             
         CLC   CONREC(2),=C'SC'                                                 
         BE    VK11                                                             
         CLI   CONREC,C'P'         PCOM NEEDS CLT                               
         BE    VK11                                                             
         CLC   CONREC(2),=C'SD'                                                 
         BNE   VK15                                                             
*                                                                               
VK11     GOTO1 ANY                                                              
*                                                                               
VK15     CLI   5(R2),0             ASSUME 'ALL' IF NO INPUT                     
         BNE   VK17                                                             
         MVC   8(3,R2),=C'ALL'                                                  
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         B     VK20                                                             
*                                                                               
VK17     CLC   =C'ALL',8(R2)                                                    
         BNE   VK18                                                             
         CLI   CONREC,C'M'         MCOM, SCOM, SDR RECS NEED A CLIENT           
         BE    INVERR                                                           
         CLI   CONREC,C'P'         PCOM NEEDS CLT                               
         BE    INVERR                                                           
         CLC   CONREC(2),=C'SC'                                                 
         BE    INVERR                                                           
         CLC   CONREC(2),=C'SD'                                                 
         BE    INVERR                                                           
         B     VK20                                                             
*                                                                               
VK18     BAS   RE,TSTOFF           TEST IF OFFICE CODE                          
         BE    VK50                DON'T SET VALID                              
         GOTO1 VALICLT                                                          
*                                                                               
VK20     OI    4(R2),X'20'                                                      
         CLC   CONREC(2),=C'AC'    ACOM?                                        
         BE    *+12                YES                                          
         CLI   CONREC,C'N'         NVTEXT                                       
         BNE   VK20A               NOPE                                         
         XC    MYPRD,MYPRD                                                      
         MVI   BEST,0                                                           
         XC    BSTA,BSTA                                                        
         B     VK40                                                             
***************                                                                 
* PRODUCT FIELD                                                                 
***************                                                                 
VK20A    LA    R2,SCOPRDH          PRODUCT FIELD                                
*        TM    4(R2),X'20'                                                      
*        BO    VK30                                                             
         NI    SCOESTH+4,X'DF'     FORCE VALIDATION OF NEXT FLD                 
         CLI   CONREC,C'M'         MCOM, SCOM, SDR COMMENTS NEED PRD            
         BE    VK21                                                             
         CLI   CONREC,C'P'         PCOM NEEDS PRD                               
         BE    VK21                                                             
         CLC   CONREC(2),=C'SC'                                                 
         BE    VK21                                                             
         CLC   CONREC(2),=C'SD'                                                 
         BNE   VK22                                                             
*                                                                               
VK21     GOTO1 ANY                                                              
*                                                                               
VK22     XC    MYPRD,MYPRD                                                      
         CLI   5(R2),0                                                          
         BNE   VK23                                                             
         CLC   =C'ALL',SCOCLT      IF NOT ALL CLIENTS AND NO PRODUCT            
         BE    VK30                                                             
         MVC   8(3,R2),=C'ALL'     THEN ASSUME 'ALL' PRODUCTS                   
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         B     VK30                                                             
*                                                                               
VK23     CLC   =C'ALL',SCOCLT      IF ALL CLIENTS                               
         BE    INVERR              THEN NOTHING AFTER IS ALLOWED                
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK24                                                             
         CLI   CONREC,C'M'         MCOM, SCOM, SDR RECS NEED A PRODUCT          
         BE    INVERR                                                           
         CLI   CONREC,C'P'         PCOM NEEDS PRD                               
         BE    INVERR                                                           
         CLC   CONREC(2),=C'SC'                                                 
         BE    INVERR                                                           
         CLC   CONREC(2),=C'SD'                                                 
         BE    INVERR                                                           
         B     VK30                                                             
*                                                                               
VK24     CLC   8(4,R2),=C'PGR='                                                 
         BNE   VK25                                                             
         CLI   CONREC,C'I'         I2COM REC CAN'T HAVE PGROUPS                 
         BE    INVERR                                                           
         CLI   CONREC,C'M'         MCOM RECORDS CAN'T HAVE PGROUPS              
         BE    INVERR                                                           
         CLI   CONREC,C'P'         PCOM RECS = NO PGROUPS                       
         BE    INVERR                                                           
         CLC   CONREC(2),=C'SC'    SCOM RECORDS CAN'T HAVE PGROUPS              
         BE    INVERR                                                           
         BAS   RE,CHKPGR           PRODUCT GROUP                                
         OI    4(R2),X'20'                                                      
         B     VK30                                                             
*                                                                               
VK25     GOTO1 VALIPRD                                                          
         CLI   CONREC,C'M'         TREAT 'POL' AS 'ALL' FOR MCOM RECS           
         BNE   VK27                                                             
         CLC   CONREC(2),=C'SC'    TREAT 'POL' AS 'ALL' FOR SCOM RECS           
         BNE   VK27                                                             
         CLI   BPRD,X'FF'                                                       
         BNE   VK27                                                             
         MVI   BPRD,0                                                           
*                                                                               
VK27     CLI   CONREC,C'I'                                                      
         BE    VK28                                                             
         MVC   MYPRD+2(1),BPRD                                                  
         B     VK30                                                             
*                                                                               
VK28     MVC   MYPRD,QPRD          CHARACTER PRD CODE FOR I2COM REC             
*                                                                               
VK30     OI    4(R2),X'20'                                                      
*                                                                               
***************                                                                 
* ESTIMATE FIELD                                                                
***************                                                                 
         LA    R2,SCOESTH          ESTIMATE FIELD                               
         TM    4(R2),X'20'                                                      
         BO    VK40                                                             
         CLI   CONREC,C'M'         MCOM, SCOM, SDR COMMENTS NEED EST            
         BE    VK31                                                             
         CLI   CONREC,C'P'         PCOM NEEDS EST                               
         BE    VK31                                                             
         CLC   CONREC(2),=C'SC'                                                 
         BE    VK31                                                             
         CLC   CONREC(2),=C'SD'                                                 
         BNE   VK35                                                             
*                                                                               
VK31     GOTO1 ANY                                                              
         NI    SMCMKTH+4,X'DF'                                                  
*                                                                               
VK35     MVI   BEST,0                                                           
         CLI   5(R2),0                                                          
         BNE   VK37                                                             
         CLC   =C'ALL',SCOCLT      IF NOT ALL CLIENTS                           
         BE    VK40                                                             
         CLC   =C'ALL',SCOPRD      OR NOT ALL PRODUCTS                          
         BE    VK40                                                             
         MVC   8(3,R2),=C'ALL'     THEN ASSUME 'ALL' ESTIMATE                   
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         B     VK40                                                             
*                                                                               
VK37     CLC   =C'ALL',SCOCLT      IF ALL CLIENTS                               
         BE    INVERR              THEN NOTHING AFTER IS ALLOWED                
*                                                                               
         CLC   =C'ALL',8(R2)       IF ALL ESTIMATES                             
         BNE   VK37A                                                            
         CLI   CONREC,C'M'         MCOM, SCOM, SDR RECS NEED ESTIMATE           
         BE    INVERR                                                           
         CLI   CONREC,C'P'         PCOM NEEDS EST                               
         BE    INVERR                                                           
         CLC   CONREC(2),=C'SC'                                                 
         BE    INVERR                                                           
         CLC   CONREC(2),=C'SD'                                                 
         BE    INVERR                                                           
         B     VK40                NOTHING TO VALIDATE                          
*                                                                               
VK37A    CLC   =C'ALL',SCOPRD      IF PRODUCT SPECIFIED                         
         BE    *+14                                                             
         CLC   =C'PGR=',SCOPRD        AND NOT PGROUP                            
         BNE   VK38                THEN VALIDATE THE ESTIMATE                   
         TM    4(R2),X'08'         WE HAVE TO VALIDATE OURSELVES                
         BZ    INVERR                                                           
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,255              ESTIMATE 1-255                               
         BH    INVERR                                                           
         LTR   R1,R1                                                            
         BZ    INVERR                                                           
         STC   R1,BEST                                                          
         B     VK40                                                             
*                                                                               
VK38     GOTO1 VALIEST                                                          
*                                                                               
VK40     OI    4(R2),X'20'                                                      
*                                                                               
***************                                                                 
* MARKET FIELD                                                                  
***************                                                                 
         CLC   CONREC(2),=C'SC'    SCOM REC?                                    
         BNE   VK40A                                                            
         CLI   SCMSTATH+5,0                                                     
         BE    *+12                                                             
         TM    SCMSTATH+4,X'08'    VALID NUMBERIC?                              
         BO    *+14                                                             
         XC    SCMMKTN,SCMMKTN     CLR MKT NAME FROM SCOM SCREEN                
         OI    SCMMKTNH+6,X'80'                                                 
*                                                                               
VK40A    CLI   CONREC,C'M'         MCOM, SCOM, BCOM RECS CAN HAVE MKT           
         BE    VK41                NOTE: SCOM'S MKT HANDLED DIFFERENTLY         
         CLI   CONREC,C'P'         AND PCOMS HAVE MKT                           
         BE    VK41                                                             
         CLI   CONREC,C'B'                                                      
         BNE   VK45                                                             
*                                                                               
VK41     LA    R2,SMCMKTH                                                       
         TM    4(R2),X'20'                                                      
         BO    VK50                                                             
         XC    SMCMKTN,SMCMKTN                                                  
         OI    SMCMKTNH+6,X'80'                                                 
         XC    BSTA,BSTA                                                        
         CLI   5(R2),0                                                          
         BE    VK50                                                             
*                                                                               
VK42     GOTO1 ANY                                                              
         GOTO1 VALIMKT                                                          
         MVC   BSTA+1(2),BMKT                                                   
         MVC   SMCMKTN,MKTNM                                                    
         OI    SMCMKTNH+6,X'80'                                                 
         B     VK50                                                             
*                                                                               
VK45     CLC   CONREC(2),=C'SD'    STATION DESCRIPTOR RECORDS NEED STAT         
         BE    VK46                                                             
         CLI   CONREC,C'I'         I2COM NEEDS STATION                          
         BE    VK46                                                             
         CLC   CONREC(2),=C'SC'    SCOM RECORDS NEED STAT                       
         BNE   VK50                                                             
*                                                                               
VK46     LA    R2,SSDSTATH                                                      
         TM    4(R2),X'20'                                                      
         BO    VK50                                                             
         XC    BSTA,BSTA                                                        
         GOTO1 ANY                                                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VK50                                                             
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BO    VK49                IT IS A MARKET INPUT                         
*                                                                               
VK47     GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
         B     VK50                                                             
*                                                                               
VK49     LA    R2,SCMSTATH         HANDLING SCOM'S MKT                          
         GOTO1 VALIMKT                                                          
         CLI   CONREC,C'I'         I2COM?                                       
         BE    VK50                YES,DONT CLOBBER MON/YR (& THEN DIE)         
         MVC   SCMMKTN,MKTNM                                                    
         OI    SCMMKTNH+6,X'80'    DISPLAY MARKET NAME ON SCOM SCREEN           
*                                                                               
VK50     LA    R6,KEY              SET UP KEY                                   
         USING COMHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   COMKTYPE,=X'0D0C'                                                
         MVC   COMKAGY,BAGYMD                                                   
         MVC   COMCTYPE,CONREC     RECORD TYPE                                  
         CLC   CONREC(2),=C'SC'    SCOM?                                        
         BE    VK55                                                             
         CLC   CONREC(2),=C'A3'                                                 
         BNE   *+8                                                              
         MVI   COMCTYPE,C'3'                                                    
         CLC   CONREC(2),=C'AC'                                                 
         BNE   *+8                                                              
         MVI   COMCTYPE,C'1'                                                    
         CLI   CONREC,C'B'         B4-B7 COMMENTS                               
         BNE   VK60                                                             
         CLC   CONREC(2),=C'BC'                                                 
         BE    VK60                                                             
         MVC   COMCTYPE,CONREC+1                                                
         B     VK60                                                             
*                                                                               
VK55     MVI   COMCTYPE,C'T'       "T" IS ASSIGNED FOR SCOM                     
         MVC   COMKCLT,BCLT                                                     
         MVC   COMKPRD,MYPRD                                                    
         MVC   COMKEST,BEST                                                     
         CLI   SCMSTATH+5,0                                                     
         BE    VK66                                                             
         TM    SCMSTATH+4,X'08'    STATION FIELD HAS MARKET ENTRY?              
         BNO   VK66                                                             
         MVC   COMKMKT,BMKT        SCOM: WHEN STA=ALL, MKT IS REQUIRED          
         B     VK80                SCOM: MKT CAN ALSO BE ALL                    
VK66     MVC   COMKSTA,BSTA                                                     
         B     VK80                                                             
*                                                                               
VK60     CLI   CONREC,C'I'                                                      
         BNE   VK70                                                             
*********************************************************************           
*        I2COM HAS TO VALIDATE ALSO MON/YR, PRD2, EST2              *           
*********************************************************************           
*                                                                               
         MVC   MYEST,BEST                                                       
         MVC   MYPRD1,MYPRD                                                     
         MVC   MYAMD,BAGYMD                                                     
         XC    BEST,BEST                                                        
         XC    MYPRD,MYPRD                                                      
*                                                                               
         LA    R2,SI2MYRH                                                       
*                                                                               
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(2,8(R2)),MYDATE                                     
         CLC   =C'000000',MYDATE                                                
         BE    INVERR                                                           
*                                                                               
VI10     LA    R2,SI2PR2H          PRODUCT 2 FIELD                              
*                                                                               
VI15     XC    MYPRD,MYPRD                                                      
         CLI   5(R2),0                                                          
         BNE   VI25                                                             
         MVC   SI2ES2,=X'404040'                                                
         OI    SI2ES2H+6,X'80'                                                  
         B     VI65                                                             
*        CLC   =C'ALL',SCOCLT      IF NOT ALL CLIENTS AND NO PRODUCT            
*        BE    VI40                                                             
*        MVC   8(3,R2),=C'ALL'     THEN ASSUME 'ALL' PRODUCTS                   
*        MVI   5(R2),3                                                          
*        OI    6(R2),X'80'                                                      
*        B     VI40                                                             
*                                                                               
*I20     CLC   =C'ALL',SCOCLT      IF ALL CLIENTS                               
*        BE    INVERR              THEN NOTHING AFTER IS ALLOWED                
*        CLC   =C'ALL',8(R2)                                                    
*        BE    VI40                                                             
*                                                                               
VI25     CLC   8(4,R2),=C'PGR='                                                 
         BNE   VI30                                                             
         BAS   RE,CHKPGR           PRODUCT GROUP                                
         B     VI40                                                             
*                                                                               
VI30     GOTO1 VALIPRD                                                          
         CLI   BPRD,X'FF'                                                       
         BNE   VI35                                                             
         MVI   BPRD,0                                                           
*                                                                               
VI35     MVC   MYPRD,QPRD                                                       
*                                                                               
VI40     LA    R2,SI2ES2H          ESTIMATE 2 FIELD                             
*                                                                               
VI50     MVI   BEST,0                                                           
         CLI   5(R2),0                                                          
         BNE   VI55                                                             
         CLC   =C'ALL',SCOCLT      IF NOT ALL CLIENTS                           
         BE    VI65                                                             
         CLC   =C'ALL',SI2PR2      OR NOT ALL PRODUCTS                          
         BE    VI65                                                             
         MVC   8(3,R2),=C'ALL'     THEN ASSUME 'ALL' ESTIMATE                   
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         B     VI65                                                             
*                                                                               
VI55     CLC   =C'ALL',SCOCLT      IF ALL CLIENTS                               
         BE    INVERR              THEN NOTHING AFTER IS ALLOWED                
*                                                                               
         CLC   =C'ALL',8(R2)       IF ALL ESTIMATES                             
         BE    VI65                                                             
*                                                                               
VI55A    CLC   =C'ALL',SI2PR2      IF PRODUCT SPECIFIED                         
         BE    *+14                                                             
         CLC   =C'PGR=',SI2PR2     AND NOT PGROUP                               
         BNE   VI60                THEN VALIDATE THE ESTIMATE                   
         TM    4(R2),X'08'         WE HAVE TO VALIDATE OURSELVES                
         BZ    INVERR                                                           
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,255              ESTIMATE 1-255                               
         BH    INVERR                                                           
         LTR   R1,R1                                                            
         BZ    INVERR                                                           
         STC   R1,BEST                                                          
         B     VI65                                                             
*                                                                               
VI60     GOTO1 VALIEST             BEST NOW CONTAINS THE 2ND ESTIMATE           
*                                                                               
VI65     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING COMI2HD,R4          BUILD XSPOT KEY FOR I2COM                    
         MVC   COMI2K,=X'0D0C'                                                  
         MVC   COMI2KAM,MYAMD                                                   
         MVI   COMI2KTY,C'I'                                                    
         MVC   COMI2KCL,BCLT                                                    
         MVC   COMI2KPR,MYPRD1                                                  
         MVC   COMI2KES,MYEST                                                   
         MVC   COMI2KP2,MYPRD                                                   
         MVC   COMI2KE2,BEST                                                    
         MVC   COMI2KST,BSTA                                                    
         GOTO1 DATCON,DMCB,(0,MYDATE),(3,COMI2KYM)                              
*                                                                               
         B     VK80                DONE WITH VALKEY                             
**********************************************************************          
VK70     MVC   COMKCLT,BCLT                                                     
         MVC   COMKPRD,MYPRD                                                    
         MVC   COMKEST,BEST                                                     
         MVC   COMKSTA,BSTA                                                     
VK80     MVC   SVRECNUM,TWALREC                                                 
*                                                                               
VKX      CLI   CONREC,C'I'                                                      
         BNE   EXIT                                                             
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SIZEIO,=F'3975'                                                  
         MVC   SYSFIL,=C'XSPFIL '   I2COM RECORDS LIVES ON XFILE                
         MVC   SYSDIR,=C'XSPDIR '                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        TEST FOR OFFICE CODE                                                   
*        R2 A(CLIENT FLD)                                                       
*                                                                               
TSTOFF   NTR1                                                                   
         CLI   5(R2),2                                                          
         BL    TONO                                                             
         CLI   5(R2),3                                                          
         BH    TONO                                                             
         CLI   8(R2),C'*'                                                       
         BNE   TONO                                                             
*                                                                               
O        USING OFFICED,WORK                                                     
         XC    WORK,WORK                                                        
         MVI   O.OFCSYS,C'S'                                                    
         MVC   O.OFCAGY,AGENCY                                                  
         MVC   O.OFCOFC2,9(R2)                                                  
         GOTO1 OFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                            
         TM    O.OFCINDS,OFCIOINV  TEST INVALID OFFICE                          
         BNZ   TONO                                                             
*&&DO                                                                           
         CLI   9(R2),C'A'                                                       
         BL    INVERR                                                           
         CLI   9(R2),C'Z'                                                       
         BNH   TO10                                                             
         CLI   9(R2),C'0'                                                       
         BL    INVERR                                                           
         CLI   9(R2),C'9'                                                       
         BH    INVERR                                                           
*&&                                                                             
*                                                                               
TO10     CLI   CONREC,C'M'        OFFICE NOT ALLOWED IN MCOM                    
         BE    INVERR                                                           
         CLI   CONREC,C'P'        OFFICE NOT ALLOWED IN PCOM                    
         BE    INVERR                                                           
         CLC   CONREC(2),=C'SC'   OFFICE NOT ALLOWED IN SCOM                    
         BE    INVERR                                                           
         CLI   CONREC,C'B'        OFFICE NOT ALLOWED IN B/B4/5/6/7COM           
         BE    INVERR                                                           
*                                                                               
**NOP    MVC   BCLT,8(R2)         OFFICE CODE IS VALID                          
         MVI   BCLT,C'*'                                                        
         MVC   BCLT+1(1),O.OFCOFC   OFFICE CODE IS VALID                        
         XC    MYPRD,MYPRD                                                      
         MVI   BEST,0                                                           
*                                                                               
         LA    R2,SCOPRDH                                                       
         CLI   5(R2),0                                                          
         BNE   INVERR              NO MORE FIELDS ALLOWED                       
         LA    R2,SCOESTH                                                       
         CLI   5(R2),0                                                          
         BNE   INVERR                                                           
         B     YES                                                              
*                                                                               
TONO     B     NO                                                               
         DROP  O                                                                
         EJECT                                                                  
***********************************************************************         
* CHECK PRODUCT GROUP                                                           
***********************************************************************         
CHKPGR   NTR1                                                                   
         CLI   5(R2),5                                                          
         BL    INVERR                                                           
         BAS   RE,GETGRP                                                        
         MVC   MYPRD,FULL                                                       
         CLI   HALF+1,0                                                         
         BE    CPG10                                                            
         OC    FULL+1(2),FULL+1    TEST GROUP NUM ENTERED                       
         BZ    INVERR                                                           
         BAS   RE,RDPGRDEF                                                      
*                                                                               
         CLC   SVBKLNS(1),HALF+1                                                
         BNE   INVERR                                                           
*                                                                               
CPG10    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),BAGYMD         A/M                                      
         MVC   KEY+3(2),BCLT           CLT                                      
         MVC   KEY+5(3),MYPRD          PGRP                                     
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE      TY/A-M/CLT/PGRPID                            
         BNE   INVERR                                                           
*                                                                               
         CLI   HALF+1,0            TEST ADDING DEFAULT                          
         BE    CPGX                                                             
*                                                                               
         MVC   FULL+1(2),KEY+6     TRANSLATE THE PGROUP NUMBER                  
         UNPK  WORK(5),FULL+1(3)                                                
         LLC   R1,HALF+1                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),8+5(R2)     MAKE SURE SPECIFIED LEVEL 1 EXISTS           
         BNE   INVERR                                                           
*                                                                               
CPGX     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* EDIT GROUP CODE - FORMAT IS A999                                              
*                                                                               
GETGRP   NTR1                                                                   
         LA    R4,12(R2)           BUMP PAST PGR=                               
         XC    FULL,FULL                                                        
         XC    HALF,HALF                                                        
*                                                                               
         LLC   R5,5(R2)                                                         
         SHI   R5,4                SUBTRACT PGR=                                
         BZ    GGX                                                              
         CHI   R5,4                MAX OF 4 CHARS                               
         BH    INVERR                                                           
         CLI   0(R4),C'A'                                                       
         BL    INVERR                                                           
         CLI   0(R4),C'Z'                                                       
         BH    INVERR                                                           
         MVC   FULL(1),0(R4)       MOVE GROUP ID                                
*                                                                               
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BZ    GGX                                                              
*                                                                               
         CLC   0(3,R4),=C'999'   MAY NOT ENTER ALL 9'S                          
         BE    INVERR                                                           
*                                                                               
         STC   R5,HALF+1           RETURN NUMBER OF DIGITS ENTERED              
         STM   R4,R5,WORK                                                       
*                                                                               
GG10     CLI   0(R4),C'0'                                                       
         BL    INVERR                                                           
         CLI   0(R4),C'9'                                                       
         BH    INVERR                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,GG10                                                          
         LM    R4,R5,WORK                                                       
         XC    WORK,WORK                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R4) *EXECUTED*                                         
         PACK  FULL+1(3),WORK(5)   GET DIGITS LEFT ALIGNED                      
*                                                                               
GGX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*     CHECK THAT NUMBER OF INPUT DIGITS = BREAK 1 DIGITS                        
*     HALF+1(1) HAS NUMBER OF DIGITS INPUT FOR GROUP                            
*                                                                               
RDPGRDEF NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),BAGYMD     A-M/CLT/PGRPID                               
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),FULL                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVERR                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING PRGRECD,R4          NB- '01' ELEM DSECTS ARE IDENTICAL           
         LA    R6,PRGEL                                                         
         USING PRGEL01,R6                                                       
*                                                                               
         LA    RF,SVBKLNS          SAVE AREA FOR PGRDEF BREAK LENGTHS           
         MVC   0(1,RF),PRGBK1LN                                                 
         MVC   1(1,RF),PRGBK2LN                                                 
         MVC   2(1,RF),PRGBK3LN                                                 
*                                                                               
RPX      MVC   AIO,AIO1                                                         
         B     EXIT                EXIT WITH CC SET                             
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         CLI   CONREC,C'I'                                                      
         BNE   VR01                                                             
         MVC   LKEY,=H'32'                                                      
         MVC   DATADISP,=H'42'     POINTS TO FIRST ELEMENT                      
         MVC   SIZEIO,=F'3975'                                                  
         MVC   SYSFIL,=C'XSPFIL'   I2COM RECORDS LIVES ON XFILE                 
         MVC   SYSDIR,=C'XSPDIR'                                                
VR01     LA    R7,MAXLNS           MAX NUMBER OF LINES                          
*                                                                               
         CLI   CONREC,C'M'         MCOM ONLY HAS 10 LINES                       
         BE    VR01A                                                            
         CLI   CONREC,C'P'         PCOM ONLY HAS 10 LINES                       
         BE    VR01A                                                            
         CLC   CONREC(2),=C'SC'    SCOM ONLY HAS 10 LINES                       
         BE    VR01A                                                            
         CLC   CONREC(2),=C'AC'    ACOM ONLY HAS 10 LINES                       
         BNE   VR01B                                                            
*                                                                               
VR01A    LA    R7,MAXMCLNS                                                      
*                                                                               
VR01B    STC   R7,MXLINES                                                       
         MVI   ELCODE,X'05'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'15'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         USING COMHDRD,R6                                                       
         L     R6,AIO                                                           
         CLI   ACTNUM,ACTADD       IS THIS AN ADD                               
         BNE   VR05                                                             
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'010C'    ADD 01 ELEMENT                               
         GOTO1 DATCON,DMCB,(5,0),(3,DUB)  CREATION DAY                          
         GOTO1 ADDELEM                                                          
         LH    R4,DATADISP         DATADISP = 24 FOR SPOT                       
         AR    R4,R6                        = 42 FOR XSPOT                      
         MVC   2(3,R4),DUB         DATADISP+2(AIO)                              
*        MVC   COMCREAT,DUB                                                     
*                                                                               
VR05     GOTO1 DATCON,DMCB,(5,0),(3,DUB) ACTIVITY DATE                          
         LH    R4,DATADISP         DATADISP = 24 FOR SPOT                       
         AR    R4,R6                        = 42 FOR XSPOT                      
         MVC   5(3,R4),DUB         DATADISP+5(AIO)                              
*        MVC   COMACTIV,DUB                                                     
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   *+12                                                             
         LA    R2,SCMPROFH                                                      
         B     VR07                                                             
         CLI   CONREC,C'I'         I2COM RECS?                                  
         BNE   *+12                                                             
         LA    R2,SI2PROFH                                                      
         B     *+8                                                              
         LA    R2,SCOPROFH                                                      
VR07     CLI   5(R2),0                                                          
         BE    *+8                                                              
         BAS   RE,EDTPROF                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                WE JUST ADDED IT                             
         LLC   R1,1(R6)            BUMP PAST 01 ELEMENT                         
         AR    R6,R1                                                            
*                                                                               
         MVI   TBFLAG,0            CLEAR FLAG                                   
         BAS   RE,CNTLN            COUNT NUMBER OF LINES/ELEMENTS               
         MVI   ELSADDED,0                                                       
*                                                                               
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   *+12                                                             
         LA    R2,SCMLN1H          LINE 1 FROM SCOM SCREEN                      
         B     VR09                                                             
         CLI   CONREC,C'I'         I2COM RECS?                                  
         BNE   *+12                                                             
         LA    R2,SI2LN1H          LINE 1 FROM I2COM SCREEN                     
         B     *+8                                                              
         LA    R2,SCOLN1H                                                       
VR09     MVI   TOPBOT,0            CLEAR FLAG                                   
         LLC   R7,MXLINES          MAX NUMBER OF LINES                          
         MVI   ELCODE,X'05'        SET FOR TOP LINES                            
*                                                                               
VR10     CLI   5(R2),0             ANY INPUT ON THIS LINE                       
         BE    VR30                                                             
         BAS   RE,CHKTB            CHECK IF TOP/BOTTOM                          
         BE    VR50                                                             
*                                                                               
VR20     XC    ELEM,ELEM                                                        
         LLC   R1,5(R2)            GET LENGTH OF INPUT                          
         LA    R3,2(R1)            L'ELEMENT                                    
         STC   R3,ELEM+1                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+2(0),8(R2)                                                  
         B     VR40                                                             
*                                                                               
VR30     CLI   NUMELS,0            IF THERE IS NOTHING ON THE SCREEN            
         BE    VR50                DON'T ADD A BLANK ELEMENT                    
         CLC   ELSADDED,NUMELS     IF ALREADY ADDED ALL ELEMENTS                
         BE    VRX                 DON'T ADD MORE                               
         CLC   ELSADDED,MXLINES                                                 
         BE    VRX                                                              
         XC    ELEM,ELEM           BLANK LINE ELEMENT                           
         MVC   ELEM+1(2),=X'0300'                                               
*                                                                               
VR40     CLI   0(R6),0             IF AT END OF RECORD                          
         BE    VR45                                                             
         CLI   0(R6),X'F1'         OR ACTIVITY ELEMENT - ADD ELEMENT            
         BE    VR45                                                             
         LLC   R1,1(R6)            ELSE BUMP PAST CURRENT ELEMENT               
         AR    R6,R1                                                            
*                                                                               
VR45     MVC   ELEM(1),ELCODE                                                   
         CLI   CONREC,C'I'                                                      
         BE    VR47                                                             
         GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R6)                                     
         B     VR48                                                             
VR47     GOTO1 RECUP,DMCB,(X'FE',AIO),ELEM,(R6),RECSPC                          
*                                                                               
VR48     LLC   R1,ELSADDED                                                      
         LA    R1,1(R1)                                                         
         STC   R1,ELSADDED                                                      
*                                                                               
VR50     BAS   RE,NXTUN            SKIP TO NEXT UNPROTECTED LINE                
         BCT   R7,VR10                                                          
*                                                                               
VRX      CLI   PFKEY,0             WAS A PF KEY HIT                             
         BE    *+8                                                              
         BAS   RE,CHKPF            YES CHECK IT                                 
         B     DR                                                               
         EJECT                                                                  
*                                                                               
*        EDIT PROFILE                                                           
*                                                                               
EDTPROF  NTR1                                                                   
         XC    BLOCK,BLOCK                                                      
         GOTO1 SCANNER,DMCB,(R2),BLOCK                                          
         LA    R3,BLOCK                                                         
*                                                                               
EP10     CLI   0(R3),0             ANYTHING THERE                               
         BE    EPX                                                              
         CLC   12(10,R3),=CL10'PAGE'                                            
         BNE   EP20                                                             
         CLC   22(10,R3),=CL10'FIRST'                                           
         BNE   *+10                                                             
         LH    R4,DATADISP                                                      
         AR    R4,R6                                                            
         NI    8(R4),X'7F'                                                      
*        NI    COMPROF1,X'7F'                                                   
         B     EP30                                                             
         CLC   22(10,R3),=CL10'ALL'                                             
         BNE   INVERR                                                           
         OI    8(R4),X'7F'                                                      
*        OI    COMPROF1,X'80'                                                   
         B     EP30                                                             
*                                  INSERT FURTHER OPTIONS HERE                  
EP20     B     INVERR                                                           
*                                                                               
EP30     LA    R3,32(R3)           BUMP TO NEXT ENTRY                           
         B     EP10                                                             
*                                                                               
EPX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        SEE IF THIS IS A PRINT=TOP/PRINT=BOTTOM LINE                           
*                                                                               
CHKTB    NTR1                                                                   
         CLC   8(9,R2),=C'PRINT=TOP'                                            
         BNE   CT10                                                             
         TM    TOPBOT,TBTOP        IS THERE ALREADY A PRINT=TOP LINE            
         BO    INVERR                                                           
         OI    TOPBOT,TBTOP        SET THERE ARE TOP LINES                      
         MVI   ELCODE,X'05'        SET FOR TOP LINES                            
         B     YES                                                              
*                                                                               
CT10     CLC   8(12,R2),=C'PRINT=BOTTOM'                                        
         BNE   NO                                                               
         TM    TOPBOT,TBBOT        IS THERE ALREADY A PRINT=BOT LINE            
         BO    INVERR                                                           
         OI    TOPBOT,TBBOT        SET THERE ARE BOTTOM LINES                   
         MVI   ELCODE,X'15'        SET ELCODE TO BOTTOM LINES                   
         B     YES                                                              
         EJECT                                                                  
*                                                                               
CHKPF    NTR1                                                                   
         BAS   RE,CNTLN            COUNT NUMBER OF LINES/ELEMENTS               
         MVI   TOPBOT,0            CLEAR FLAG                                   
         CLI   PFKEY,3             ERASE LINE?                                  
         BE    CP05                                                             
         CLI   PFKEY,4             ADD A LINE?                                  
         BNE   CPX                                                              
         CLC   NUMLNS,MXLINES                                                   
         BE    TOOMANY                                                          
*                                                                               
CP05     L     RE,SYSPARMS                                                      
         L     R3,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,R3                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       ABSOLUTE CURSOR ADDRESS                      
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         MH    R1,=H'80'           ABSOLUTE ADDR OF BEGINNING OF LINE           
         DROP  R3                                                               
*                                                                               
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   *+12                                                             
         LA    R2,SCMLN1H                                                       
         B     CP07                                                             
         CLI   CONREC,C'I'         I2COM RECS?                                  
         BNE   *+12                                                             
         LA    R2,SI2LN1H                                                       
         B     *+8                                                              
         LA    R2,SCOLN1H          1ST FIELD WHICH COULD CONTAIN CURSOR         
CP07     L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    CP10                                                             
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL                                                         
         BNE   CPX                                                              
*                                                                               
CP10     SR    RF,RF                                                            
         SR    RF,RF                                                            
         ICM   RF,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'           ABSOLUTE SCREEN ADDR OF LINE START           
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   *+12                                                             
         LA    RE,L'SCMLN1(RF)                                                  
         B     CP10A                                                            
         CLI   CONREC,C'I'         I2COM RECS?                                  
         BNE   *+12                                                             
         LA    RE,L'SI2LN1(RF)                                                  
         B     *+8                                                              
         LA    RE,L'SCOLN1(RF)     ABSOLUTE SCREEN ADDR OF LINE END             
*                                                                               
CP10A    CR    RF,R1               WAS CURSOR ON THIS LINE?                     
         BH    CPX                 NO - IT'S ABOVE THIS FIELD - DONE            
         CR    RE,R1                                                            
         BNL   CP20                YES                                          
*                                                                               
         MVI   TOPBOT,0            CLEAR FLAG                                   
         BAS   RE,CHKTB            IS THIS A PRINT= LINE                        
         BNE   CP11                                                             
         TM    TOPBOT,TBBOT        IS THIS BOTTOM                               
         BZ    CP15                NO - IT'S TOP SO SKIP TO NEXT LINE           
         L     R6,AIO                                                           
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL                                                         
         BNE   CPX                                                              
         B     CP15                                                             
*                                                                               
CP11     LR    R3,R6               SAVE LAST A(ELEMENT)                         
         BAS   RE,NEXTEL           BUMP TO NEXT ELEMENT                         
         BE    CP15                                                             
         LR    R6,R3                                                            
*                                                                               
CP15     BAS   RE,NXTUN            BUMP TO NEXT UNPROTECTED FIELD               
         LA    RF,SCOLN14H                                                      
*                                                                               
         CLI   CONREC,C'M'         MCOM ONLY HAVE 10 LINES                      
         BNE   *+8                                                              
         LA    RF,SMCLN10H                                                      
         CLC   CONREC(2),=C'SC'    SCOM ONLY HAVE 10 LINES                      
         BNE   *+8                                                              
         LA    RF,SCMLN10H                                                      
         CLI   CONREC,C'I'         I2COM                                        
         BNE   *+8                                                              
         LA    RF,SI2LN14H                                                      
*                                                                               
         CR    R2,RF               END OF SCREEN?                               
         BH    CPX                 YES - IGNORE                                 
         B     CP10                                                             
*                                                                               
CP20     LA    RF,SCOLN14H         A(LAST TEXT FIELD)                           
*                                                                               
         CLI   CONREC,C'M'         MCOM ONLY HAVE 10 LINES                      
         BNE   *+8                                                              
         LA    RF,SMCLN10H                                                      
         CLC   CONREC(2),=C'SC'    MCOM ONLY HAVE 10 LINES                      
         BNE   *+8                                                              
         LA    RF,SCMLN10H                                                      
         CLI   CONREC,C'I'         I2COM                                        
         BNE   *+8                                                              
         LA    RF,SI2LN14H                                                      
*                                                                               
         CLI   PFKEY,3             ERASE LINE?                                  
         BNE   CP50                                                             
         CLI   CONREC,C'I'                                                      
         BE    CP25                                                             
         GOTO1 RECUP,DMCB,(0,AIO),(R6),(R6)                                     
         B     CP60                                                             
CP25     GOTO1 RECUP,DMCB,(X'FE',AIO),(R6),(R6),RECSPC                          
         B     CP60                                                             
*                                                                               
CP50     LLC   R1,1(R6)            GET START OF NEXT ELEMENT                    
         AR    R6,R1                                                            
         MVC   ELEM,ELCODE                                                      
         MVC   ELEM+1(2),=X'0300'                                               
         CLI   CONREC,C'I'                                                      
         BNE   CP55                                                             
         GOTO1 RECUP,DMCB,(X'FE',AIO),ELEM,(R6),RECSPC                          
         B     CP57                                                             
CP55     GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R6)                                     
CP57     BAS   RE,NXTUN            BUMP TO NEXT UNPROTECED FIELD                
*                                                                               
CP60     ST    R2,ACURFORC         KEEP CURSOR IN PLACE                         
*                                                                               
CPX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        COUNT NUMBER OF LINES INPUT                                            
*                                                                               
CNTLN    NTR1                                                                   
         MVI   TBFLAG,0                                                         
         MVI   TOPBOT,0                                                         
*                                                                               
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   *+12                                                             
         LA    R2,SCMLN1H                                                       
         B     CL05                                                             
         CLI   CONREC,C'I'         I2COM REC                                    
         BNE   *+12                                                             
         LA    R2,SI2LN1H                                                       
         B     *+8                                                              
*                                                                               
CL05     LA    R2,SCOLN1H                                                       
         MVI   NUMELS,0                                                         
         MVI   NUMLNS,0                                                         
         SR    R3,R3                                                            
         SR    R4,R4                                                            
         LLC   R7,MXLINES          MAX NUMBER OF LINES                          
*                                                                               
CL10     LA    R4,1(R4)            CURRENT LINE NUMBER                          
         CLI   5(R2),0             ANY INPUT ON THIS LINE                       
         BE    CL20                                                             
         STC   R4,NUMELS                                                        
         OC    TBFLAG,TOPBOT       KEEP FLAG                                    
         MVI   TOPBOT,0            CLEAR FLAG                                   
         BAS   RE,CHKTB            CHECK IF TOP/BOTTOM                          
         BE    CL20                                                             
         LA    R3,1(R3)            INC COUNTER                                  
*                                                                               
CL20     BAS   RE,NXTUN            SKIP TO NEXT UNPROTECTED LINE                
         BCT   R7,CL10                                                          
         MVC   NUMLNS,NUMELS                                                    
         LLC   R4,NUMELS                                                        
         TM    TBFLAG,TBTOP        IS THERE PRINT=TOP                           
         BNO   *+6                                                              
         BCTR  R4,0                                                             
         TM    TBFLAG,TBBOT        IS THERE PRINT=BOTTOM                        
         BNO   *+6                                                              
         BCTR  R4,0                                                             
         STC   R4,NUMELS                                                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       CLC   CONREC(2),=C'SC'                                                 
         BNE   DR00                                                             
         TWAXC SCMLN1H                                                          
         B     DR00B                                                            
DR00     CLI   CONREC,C'I'         I2COM RECS?                                  
         BNE   DR00A                                                            
         TWAXC SI2LN1H                                                          
         B     DR00B                                                            
*                                                                               
DR00A    TWAXC SCOLN1H                                                          
*        USING COMHDRD,R6                                                       
DR00B    L     R6,AIO                                                           
         LH    R4,DATADISP                                                      
         AR    R4,R6                                                            
*                                                                               
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   *+12                                                             
         LA    R2,SCMPROF          PROFILE FIELD FROM SCOM SCREEN               
         B     DR01                                                             
         CLI   CONREC,C'I'                                                      
         BNE   *+12                                                             
         LA    R2,SI2PROF                                                       
         B     *+8                                                              
         LA    R2,SCOPROF                                                       
DR01     TM    8(R4),X'80'         PAGE=ALL BIT?                                
*        TM    COMPROF1,X'80'      PAGE=ALL BIT                                 
         BNO   *+10                                                             
         MVC   0(8,R2),=C'PAGE=ALL'                                             
*                                                                               
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   DR01A                                                            
         GOTO1 DATCON,DMCB,(3,2(R4)),(5,SCMCDTE)                                
         GOTO1 DATCON,DMCB,(3,5(R4)),(5,SCMADTE)                                
         B     DR03                                                             
DR01A    CLI   CONREC,C'I'         I2COM RECS?                                  
         BNE   DR02                                                             
         GOTO1 DATCON,DMCB,(3,2(R4)),(5,SI2CDTE)                                
         GOTO1 DATCON,DMCB,(3,5(R4)),(5,SI2ADTE)                                
         B     DR03                                                             
DR02     GOTO1 DATCON,DMCB,(3,2(R4)),(5,SCOCDTE)                                
         GOTO1 DATCON,DMCB,(3,5(R4)),(5,SCOADTE)                                
*                                                                               
DR03     CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   *+12                                                             
         LA    R2,SCMLN1H                                                       
         B     DR05                                                             
         CLI   CONREC,C'I'         I2COM RECS?                                  
         BNE   *+12                                                             
         LA    R2,SI2LN1H                                                       
         B     *+8                                                              
         LA    R2,SCOLN1H                                                       
DR05     MVI   TOPBOT,0            CLEAR FLAG                                   
         LLC   R7,MXLINES          LINE COUNTER                                 
         MVI   ELCODE,X'15'        ARE THERE ARE BOTTOM LINES                   
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    TOPBOT,TBBOT        YES                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        ANY TOP LINES                                
         BAS   RE,GETEL                                                         
         BNE   DR20                                                             
         OI    TOPBOT,TBTOP        SET THERE ARE TOP LINES                      
         TM    TOPBOT,TBBOT        ARE THERE BOTTOM LINES                       
         BZ    DR10                                                             
         MVC   8(9,R2),=C'PRINT=TOP'                                            
         BCTR  R7,0                DECREMENT NUM LINES                          
         BAS   RE,NXTUN            FIND NEXT UNPROTECTED FIELD                  
*                                                                               
DR10     BAS   RE,PRNTOUT                                                       
*                                                                               
DR20     TM    TOPBOT,TBBOT        ARE THERE BOTTOM LINES                       
         BZ    DR30                                                             
         MVC   8(12,R2),=C'PRINT=BOTTOM'                                        
         BCTR  R7,0                DECREMENT NUM LINES                          
         BAS   RE,NXTUN            FIND NEXT UNPROTECTED FIELD                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'15'        ANY BOT LINES                                
         BAS   RE,GETEL                                                         
         BNE   DR30                                                             
         BAS   RE,PRNTOUT                                                       
*                                                                               
DR30     L     R6,AIO                                                           
*                                                                               
DRX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        PUT COMMENTS TO LINE                                                   
*                                                                               
PRNTOUT  NTR1                                                                   
*                                                                               
PO10     MVI   8(R2),C' '          CLEAR LINE                                   
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   PO12                                                             
         MVC   9(L'SCMLN1-1,R2),8(R2)                                           
         B     PO20                                                             
PO12     CLI   CONREC,C'I'         I2COM RECS?                                  
         BNE   PO15                                                             
         MVC   9(L'SI2LN1-1,R2),8(R2)                                           
         B     PO20                                                             
PO15     MVC   9(L'SCOLN1-1,R2),8(R2)                                           
*                                                                               
PO20     LLC   R1,1(R6)            ELEMENT LENGTH                               
         SHI   R1,3                SUB ELCODE/ELEM LENGTH/& 1 FOR EX            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),2(R6)                                                    
         BAS   RE,NXTUN            FIND NEXT UNPROTECTED FIELD                  
         ICM   R1,15,SCOLN14H                                                   
*                                                                               
         CLI   CONREC,C'M'         MCOM ONLY HAVE 10 LINES                      
         BNE   *+8                                                              
         ICM   R1,15,SMCLN10H                                                   
         CLC   CONREC(2),=C'SC'    SCOM ONLY HAVE 10 LINES                      
         BNE   *+8                                                              
         ICM   R1,15,SCMLN10H                                                   
         CLI   CONREC,C'I'         I2COM HAVE 14 LINES BUT DIFFERENT            
         BNE   *+8                                                              
         ICM   R1,15,SI2LN14H                                                   
*                                                                               
         CR    R2,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    PO10                                                             
*                                                                               
POX      XIT1  REGS=(R2,R7)                                                     
*                                                                               
*        FIND NEXT UNPROTECTED FIELD                                            
*                                                                               
NXTUN    LLC   RF,0(R2)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                 END OF SCREEN                                
         DC    H'0'                                                             
         AR    R2,RF                                                            
         TM    1(R2),X'20'         IS THIS PROTECTED                            
         BO    NXTUN               YES GET NEXT FIELD                           
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        SET FILE                                                               
*                                                                               
SETFIL   NTR1                                                                   
SF       CLI   CONREC,C'I'                                                      
         BNE   SFX                                                              
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SIZEIO,=F'3975'                                                  
         MVC   SYSFIL,=C'XSPFIL'                                                
         MVC   SYSDIR,=C'XSPDIR'                                                
SFX      B     EXIT                                                             
*                                                                               
*        DISPLAY KEY                                                            
*                                                                               
DK       DS    0H                                                               
         CLI   CONREC,C'I'                                                      
         BE    DKI                                                              
*                                                                               
DK01     L     R6,AIO                                                           
         USING COMHDRD,R6                                                       
         MVC   WKAGY,COMKAGY       SAVE KEY'S COMPONENTS                        
         MVC   WKCTYPE,COMCTYPE                                                 
         MVC   WKCLT,COMKCLT                                                    
         MVC   WKPRD,COMKPRD                                                    
         MVC   WKEST,COMKEST                                                    
         MVC   WKSTA,COMKSTA                                                    
*                                                                               
DK02     LLC   RE,COMKAGY                                                       
         SLL   RE,28                                                            
         SRL   RE,28               ISOLATE MEDIA                                
         IC    RE,MEDTAB-1(RE)                                                  
         STC   RE,SCOMED                                                        
*                                                                               
         OC    COMKCLT,COMKCLT                                                  
         BNZ   DK03                                                             
         MVC   SCOCLT(3),=C'ALL'                                                
         B     DK05                                                             
*                                                                               
DK03     TM    COMKCLT,X'80'       TEST THIS IS AN OFFICE                       
         BZ    DK04                                                             
         BAS   RE,GETAAN                                                        
         GOTO1 CLUNPK,DMCB,(CLTAAN,COMKCLT),SCOCLT                              
         B     DK05                                                             
*                                                                               
O        USING OFFICED,WORK                                                     
DK04     XC    WORK,WORK                                                        
         MVI   O.OFCSYS,C'S'                                                    
         MVC   O.OFCAGY,AGENCY                                                  
         MVC   O.OFCOFC,COMKCLT+1                                               
         GOTO1 OFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                            
         MVI   SCOCLT,C'*'                                                      
         MVC   SCOCLT+1(2),O.OFCOFC2                                            
         DROP  O                                                                
*                                                                               
DK05     MVI   SCOPRD,C' '                                                      
         MVC   SCOPRD+1(L'SCOPRD-1),SCOPRD                                      
*                                                                               
         OC    COMKPRD,COMKPRD                                                  
         BNZ   DK07                                                             
*                                                                               
         CLI   CONREC,C'M'         MCOM RECORD?                                 
         BNE   *+14                                                             
         MVC   SCOPRD(3),=C'POL'   TREAT 'POL' AS 'ALL'                         
         B     DK10                                                             
         CLC   CONREC(2),=C'SC'    SCOM RECORD?                                 
         BNE   *+14                                                             
         MVC   SCOPRD(3),=C'POL'   TREAT 'POL' AS 'ALL'                         
         B     DK10                                                             
*                                                                               
         CLC   =C'ALL',SCOCLT                                                   
         BE    DK10                                                             
         MVC   SCOPRD(3),=C'ALL'                                                
         B     DK10                                                             
*                                                                               
DK07     CLI   CONREC,C'I'                                                      
         BNE   DK08                                                             
         MVC   SCOPRD,COMKPRD      THIS IS ACTUALLY COMI2KPRD                   
         B     DK10                                                             
DK08     BAS   RE,GETPRD                                                        
         MVC   SCOPRD,WORK                                                      
*                                                                               
DK10     MVI   SCOEST,C' '                                                      
         MVC   SCOEST+1(L'SCOEST-1),SCOEST                                      
         CLI   WKEST,0                                                          
         BNE   DK15                                                             
         CLC   =C'ALL',SCOCLT                                                   
         BE    DK20                                                             
         CLC   =C'ALL',SCOPRD                                                   
         BE    DK20                                                             
         MVC   SCOEST(3),=C'ALL'                                                
         B     DK20                                                             
DK15     LLC   R0,WKEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SCOEST,DUB                                                       
*                                                                               
DK20     DS    0H                                                               
         OC    WKSTA,WKSTA                                                      
         BZ    DK50                                                             
         CLI   CONREC,C'B'                                                      
         BE    DK25                                                             
         CLI   CONREC,C'M'         MCOM                                         
         BE    DK25                                                             
         CLI   CONREC,C'P'         PCOM                                         
         BE    DK25                                                             
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   DK30                                                             
         CLI   WKSTA,X'00'         IS IT MKT?                                   
         BNE   DK30                                                             
*                                                                               
DK25     SR    R1,R1               DISPLAY MARKET                               
         ICM   R1,3,WKSTA+1                                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
*                                                                               
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   DK26                                                             
         XC    SCMSTAT,SCMSTAT                                                  
         UNPK  SCMSTAT(4),DUB                                                   
         MVI   SCMSTATH+5,4        SET LENGTH                                   
         OI    SCMSTATH+4,X'08'    SET VALID NUMERIC                            
         OI    SCMSTATH+6,X'80'    XMIT                                         
         LA    R2,SCMSTATH                                                      
         B     DK27                                                             
*                                                                               
DK26     UNPK  SMCMKT(4),DUB                                                    
         MVI   SMCMKTH+5,4         SET LENGTH                                   
         OI    SMCMKTH+4,X'08'     SET VALID NUMERIC                            
         OI    SMCMKTH+6,X'80'     XMIT                                         
         LA    R2,SMCMKTH                                                       
*                                                                               
DK27     MVI   USEIONUM,2          USE AIO2                                     
         GOTO1 VALIMKT             GET MARKET NAME                              
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   DK29                                                             
         MVC   SCMMKTN,MKTNM                                                    
         OI    SCMMKTNH+6,X'80'                                                 
         MVC   AIO,AIO1            RESET AIO                                    
         B     DKX                                                              
*                                                                               
DK29     MVC   SMCMKTN,MKTNM                                                    
         OI    SMCMKTNH+6,X'80'                                                 
         MVC   AIO,AIO1            RESET AIO                                    
         B     DKX                                                              
*                                                                               
DK30     XC    WORK,WORK                                                        
         MVC   WORK+12(3),WKSTA                                                 
         GOTO1 MSUNPK,DMCB,(X'80',WORK+10),DUB,WORK                             
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   DK35                                                             
         XC    SCMMKTN,SCMMKTN     CLR PREVIOUS CONTENT OF MKT NAME             
         OI    SCMMKTNH+6,X'80'                                                 
*                                                                               
DK35     MVC   SSDSTAT,WORK                                                     
         CLI   WKSTA,X'F0'         CABLE HEADEND?                               
         BL    *+12                NO                                           
         MVI   SSDSTAT+4,C'/'      YES - WE NEED THE /                          
         B     DK50                                                             
*                                                                               
         LA    R1,SSDSTAT+3                                                     
         CLI   0(R1),C' '                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'-'                                                       
         MVC   2(1,R1),WORK+4                                                   
*                                                                               
DK50     CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   DKX                                                              
         XC    SCMMKTN,SCMMKTN     CLEAR MARKET NAME                            
         OI    SCMMKTNH+6,X'80'                                                 
         B     DKX                                                              
*                                                                               
DKI      DS    0H                                                               
         L     R6,AIO                                                           
         USING COMI2HD,R6                                                       
*                                                                               
         MVC   WKI2KAM,COMI2KAM                                                 
         MVC   WKI2KCTY,COMI2KTY                                                
         MVC   WKI2KCLT,COMI2KCL                                                
         XC    WKI2KPRD,WKI2KPRD                                                
         MVC   WKI2KPRD,COMI2KPR                                                
         MVC   WKI2KEST,COMI2KES                                                
         MVC   WKI2KPR2,COMI2KP2                                                
         MVC   WKI2KES2,COMI2KE2                                                
         MVC   WKI2KST,COMI2KST                                                 
         MVC   WKI2KYM,COMI2KYM                                                 
*                                                                               
DKI02    LLC   RE,COMI2KAM                                                      
         SLL   RE,28                                                            
         SRL   RE,28                                                            
         IC    RE,MEDTAB-1(RE)                                                  
         STC   RE,SI2MED                                                        
*                                                                               
         OC    COMI2KCL,COMI2KCL                                                
         BNZ   DKI03                                                            
         MVC   SI2CLT(3),=C'ALL'                                                
         B     DKI05                                                            
*                                                                               
DKI03    TM    COMI2KCL,X'80'      TEST THIS IS AN OFFICE                       
         BZ    DKI04                                                            
         BAS   RE,GETAAN                                                        
         GOTO1 CLUNPK,DMCB,(CLTAAN,COMI2KCL),SI2CLT                             
         B     DKI05                                                            
*                                                                               
O        USING OFFICED,WORK                                                     
DKI04    XC    WORK,WORK                                                        
         MVI   O.OFCSYS,C'S'                                                    
         MVC   O.OFCAGY,AGENCY                                                  
         MVC   O.OFCOFC,COMI2KCL+1                                              
         GOTO1 OFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                            
         MVI   SI2CLT,C'*'                                                      
         MVC   SI2CLT+1(2),O.OFCOFC2                                            
         DROP  O                                                                
*                                                                               
DKI05    OC    COMI2KPR,COMI2KPR                                                
         BNZ   DKI07                                                            
*                                                                               
         CLC   =C'ALL',SI2CLT                                                   
         BE    DKI10                                                            
         MVC   SI2PRD(3),=C'ALL'                                                
         B     DK10                                                             
*                                                                               
DKI07    MVC   SI2PRD(3),COMI2KPR                                               
*                                                                               
DKI10    MVI   SI2EST,C' '                                                      
         MVC   SI2EST+1(L'SI2EST-1),SI2EST                                      
         CLI   WKI2KEST,0                                                       
         BNE   DKI15                                                            
         CLC   =C'ALL',SI2CLT                                                   
         BE    DKI20                                                            
         CLC   =C'ALL',SI2PRD                                                   
         BE    DKI20                                                            
         MVC   SI2EST(3),=C'ALL'                                                
         B     DKI20                                                            
DKI15    LLC   R0,WKI2KEST                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SI2EST,DUB                                                       
*                                                                               
DKI20    DS    0H                                                               
         OC    WKI2KST,WKI2KST                                                  
         BZ    DKI40                                                            
         XC    WORK,WORK                                                        
         MVC   WORK+12(3),WKI2KST                                               
         GOTO1 MSUNPK,DMCB,WORK+10,DUB,WORK                                     
*                                                                               
DKI30    MVC   SI2STAT,WORK                                                     
         CLI   WORK+4,C' '                                                      
         BE    DKI40                                                            
         CLI   WORK+4,C'/'                                                      
         BNE   *+14                                                             
         MVC   SI2STAT+4(1),WORK+4                                              
         B     DKI40                                                            
*                                                                               
         LA    R1,SI2STAT+3                                                     
         CLI   0(R1),C' '                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'-'                                                       
         MVC   2(1,R1),WORK+4                                                   
*                                                                               
DKI40    OC    COMI2KYM,COMI2KYM                                                
         BZ    DKI50                                                            
         GOTO1 DATCON,DMCB,(3,COMI2KYM),(6,SI2MYR)                              
*                                                                               
DKI50    OC    COMI2KP2,COMI2KP2                                                
         BZ    DKI60                                                            
         MVC   SI2PR2(3),COMI2KP2                                               
*                                                                               
DKI60    OC    COMI2KE2,COMI2KE2                                                
         BZ    DKX                                                              
*                                                                               
         LLC   R0,COMI2KE2                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SI2EST,DUB                                                       
*                                                                               
DKX      OI    SCOMEDH+6,X'80'                                                  
         OI    SCOCLTH+6,X'80'                                                  
         OI    SCOPRDH+6,X'80'                                                  
         OI    SCOESTH+6,X'80'                                                  
         OI    SSDSTATH+6,X'80'                                                 
         CLI   CONREC,C'I'                                                      
         BNE   DKX10                                                            
         OI    SI2MYRH+6,X'80'                                                  
         OI    SI2PR2H+6,X'80'                                                  
         OI    SI2ES2H+6,X'80'                                                  
         B     DKX20                                                            
*                                                                               
         DROP  R6                                                               
DKX10    XC    KEY,KEY             REBUILT KEY BEFORE EXITING DK                
         MVC   KEY(2),=X'0D0C'     NOTE: SAVED COMPNONENTS OF KEYS              
         MVC   KEY+2(1),WKAGY            ARE USED.                              
         MVC   KEY+3(1),WKCTYPE                                                 
         MVC   KEY+4(2),WKCLT                                                   
         MVC   KEY+6(3),WKPRD                                                   
         MVC   KEY+9(1),WKEST                                                   
         MVC   KEY+10(3),WKSTA                                                  
         B     DKX30                                                            
*                                                                               
DKX20    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D0C'                                                  
         MVC   KEY+2(1),WKI2KAM                                                 
         MVC   KEY+3(1),WKI2KCTY                                                
         MVC   KEY+4(2),WKI2KCLT                                                
         MVC   KEY+6(3),WKI2KPRD                                                
         MVC   KEY+9(1),WKI2KEST                                                
         MVC   KEY+10(3),WKI2KPR2                                               
         MVC   KEY+13(1),WKI2KES2                                               
         MVC   KEY+14(3),WKI2KST                                                
         MVC   KEY+17(2),WKI2KYM                                                
*                                                                               
DKX30    CLI   CONREC,C'I'                                                      
         BNE   DKX40                                                            
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SIZEIO,=F'3975'                                                  
         MVC   SYSFIL,=C'XSPFIL'                                                
         MVC   SYSDIR,=C'XSPDIR'                                                
DKX40    MVC   AIO,AIO1            RESET IO, (DOUBLE CHECK)                     
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   CONREC,C'I'                                                      
         BE    *+14                                                             
         CLC   KEY(13),KEYSAVE                                                  
         B     *+10                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        GET PRODUCT CODE                                                       
*                                                                               
GETPRD   NTR1                                                                   
         XC    WORK,WORK           CLEAR OUTPUT DISPLAY AREA                    
         L     R6,AIO                                                           
         USING COMHDRD,R6                                                       
         CLI   COMKPRD,0           TEST PRDGRP PRESENT                          
         BNE   GP40                YES                                          
*                                  *** PRODUCT CODE ***                         
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),COMKAGY                                                 
         MVC   KEY+2(3),COMKCLT                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO              TRANSLATE BINARY CODE                        
         LA    R1,CLIST-CLTHDRD(R4)                                             
*                                                                               
GP20     CLC   WKPRD+2(1),3(R1)                                                 
         BE    GP30                PRD NUMBER SAME, GET MNEMONIC                
         LA    R1,4(R1)                                                         
         B     GP20                                                             
         CLI   0(R1),C' '                                                       
         BNL   GP20                                                             
         LA    R1,=C'***'                                                       
*                                                                               
GP30     MVC   WORK(3),0(R1)                                                    
         MVC   KEY(13),SVKEY       RESTORE DIR FOR SEQ READING                  
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         B     GPX                                                              
*                                                                               
* DISPLAY PRODUCT GROUP *                                                       
*                                                                               
GP40     MVC   SVKEY,KEY                                                        
         MVC   BAGYMD,COMKAGY                                                   
         MVC   BCLT,COMKCLT                                                     
         MVC   FULL(3),COMKPRD                                                  
         MVI   FULL+3,0                                                         
         BAS   RE,RDPGRDEF                                                      
         MVC   WORK(4),=C'PGR='                                                 
         MVC   WORK+4(1),COMKPRD                                                
         UNPK  WORK+5(5),FULL+1(3)                                              
         LLC   R1,SVBKLNS                                                       
         LA    R1,WORK+5(R1)                                                    
         XC    0(3,R1),0(R1)                                                    
         MVC   KEY(13),SVKEY       RESTORE DIR FOR SEQ READING                  
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
GPX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
GETAAN   NTR1                                                                   
*                                  *** CPROF+6 (AAN)***                         
         MVI   CLTAAN,C'N'                                                      
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),WKAGY      AGENCY/MEDIA                                 
         MVC   KEY+2(2),WKCLT      CLIENT                                       
         CLI   CONREC,C'I'         I2COM RECORD?                                
         BNE   GAAN10              NO                                           
         MVC   KEY+1(1),WKI2KAM    YES - AGENCY/MEDIA                           
         MVC   KEY+2(2),WKI2KCLT   CLIENT                                       
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SIZEIO,=F'3975'                                                  
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
*                                                                               
GAAN10   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     HAVE CLIENT RECORD?                          
         BNE   GAAN20              NO                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   CLTAAN,CPROF+6-CLTHDRD(R6)                                       
GAAN20   BAS   RE,SETFIL                                                        
         MVC   KEY(L'SVKEY),SVKEY   RESTORE DIR FOR SEQ READING                 
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
GAANX    B     EXIT                                                             
*                                                                               
*        ADD A REQ TO REQUEST FILE                                              
*                                                                               
REQ      DS    0H                                                               
         L     R1,AIO2                                                          
         XC    0(250,R1),0(R1)                                                  
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,26(R1)                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'L6'                                                   
         MVC   2(2,R1),AGENCY                                                   
         MVC   4(1,R1),QMED                                                     
         MVC   61(1,R1),SVKEY+3    COMMENT TYPE                                 
         MVC   5(3,R1),=C'ALL'     CLIENT                                       
         MVI   8(R1),C' '          PRODUCT GROUP                                
         MVC   11(3,R1),=C'ALL'    PRODUCT                                      
         MVC   23(3,R1),=C'ALL'    ESTIMATE                                     
         CLC   SVKEY+4(2),=X'0000' CLIENT SPECIFIED                             
         BE    REQ30                                                            
         MVC   5(3,R1),QCLT        YES                                          
         CLC   SVKEY+6(3),=X'000000'  PRODUCT OR GROUP SPECIFIED                
         BE    REQ20                                                            
         CLI   SVKEY+6,X'00'       PRODUCT NUMBER USED                          
         BE    REQ10                                                            
         MVC   8(1,R1),SVKEY+6                                                  
*                                                                               
         LLC   R3,SVBKLNS                                                       
         BCTR  R3,0                                                             
         MVC   HALF,SVKEY+7                                                     
         OI    HALF+1,X'0F'                                                     
         UNPK  WORK(3),HALF                                                     
         MVI   11(R1),C' '                                                      
         MVC   12(2,R1),11(R1)                                                  
         EX    R3,*+8              SHORTEN TO BREAK LEN                         
         B     REQ20                                                            
         MVC   11(0,R1),WORK       ** EXECUTED **                               
*                                                                               
REQ10    MVC   11(3,R1),QPRD       YES                                          
*                                                                               
REQ20    CLI   SVKEY+9,X'00'       ESTIMATE SPECIFIED                           
         BE    REQ30                                                            
         LLC   R3,SVKEY+9          YES                                          
         CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R1),DUB+6(2)                                                
*                                                                               
REQ30    MVI   65(R1),C'*'                                                      
         MVC   68(7,R1),=C'CONTROL'                                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO2,AIO2                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TOOMANY  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'MAXIMUM NUMBER OF COMMENT LINES'                  
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   *+12                                                             
         LA    R2,SCMLN1H                                                       
         B     *+8                                                              
         LA    R2,SI2LN1H                                                       
         B     *+8                                                              
         LA    R2,SCOLN1H                                                       
         B     MSGERR                                                           
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  OC    ERRDISP,ERRDISP     DO I NEED TO OVERRIDE CURSOR POS.            
         BZ    TRAPEND                                                          
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSETC   OVERRIDE CURSOR POSITION                     
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         STCM  RF,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         MVC   TIOBCURI,ERRDISP+1  DISPLACMENT INTO FIELD                       
*                                                                               
TRAPEND  GOTO1 ERREX               NEVER TO RETURN                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
MEDTAB   DC    C'TRNX...C'                                                      
ZEROES   DC    20C'0'                                                           
ERRDISP  DS    H                                                                
RECSPC   DC    AL2(42,32,3972)                                                  
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM95D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM94D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM93D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM92D          SDR                                          
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM90D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFMF4D          SCOM                                         
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM61D          I2COM                                        
         EJECT                                                                  
         ORG   CONTAGH             ACOM                                         
       ++INCLUDE SCSFM38D                                                       
         EJECT                                                                  
*                                                                               
COMHDRD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
         EJECT                                                                  
COMI2HD  DSECT                                                                  
       ++INCLUDE SPGENXCOM                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT          FOR PRODUCT MNEMONIC                         
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
MAXLNS   EQU   14                                                               
MAXMCLNS EQU   10                                                               
SVIO     DS    F                                                                
SVRECNUM DS    XL1                                                              
SVBKLNS  DS    XL3                                                              
MYAMD    DS    XL1                                                              
MYPRD    DS    XL3                                                              
MYPRD1   DS    XL3                                                              
MYEST    DS    XL1                                                              
MYDATE   DS    CL6                                                              
MXLINES  DS    XL1                                                              
NUMELS   DS    XL1                                                              
NUMLNS   DS    XL1                                                              
ELSADDED DS    XL1                                                              
TBFLAG   DS    CL1                 FLAG FOR TOP & BOTTOM LINES                  
TOPBOT   DS    CL1                 FLAG FOR TOP & BOTTOM LINES                  
TBTOP    EQU   X'80'                                                            
TBBOT    EQU   X'40'                                                            
*                                                                               
WKAGY    DS    XL1                 FOR DISPLAY KEY USE                          
WKCTYPE  DS    XL1                 CAN BE USED FOR OTHER PURPOSES TOO           
WKCLT    DS    XL2                                                              
WKPRD    DS    XL3                                                              
WKEST    DS    XL1                                                              
WKSTA    DS    XL3                                                              
*                                                                               
WKI2KAM  DS    XL1                                                              
WKI2KCTY DS    XL1                                                              
WKI2KCLT DS    XL2                                                              
WKI2KPRD DS    XL3                                                              
WKI2KEST DS    XL1                                                              
WKI2KPR2 DS    XL3                                                              
WKI2KES2 DS    XL1                                                              
WKI2KST  DS    XL3                                                              
WKI2KYM  DS    XL2                                                              
*                                                                               
CLTAAN   DS    CL1                 SAVED CPROF+6 FROM CLT RECORD                
         PRINT OFF                                                              
         SPACE 1                                                                
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPSFM45   04/26/16'                                      
         END                                                                    

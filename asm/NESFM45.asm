*          DATA SET NESFM45    AT LEVEL 208 AS OF 10/31/05                      
*PHASE T31C45A                                                                  
*INCLUDE RECUP                                                                  
*====================================================================*          
*  I2     I                        LIVES ON XSPFIL                   *          
*====================================================================*          
T31C45   TITLE 'I2 COMMENT RECORD'                                              
T31C45   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T31C45,R8                                                      
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
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        INITIALIZATIONS                                                        
*                                                                               
INIT     NTR1                                                                   
         OI    CONSERVH+6,X'81'    FORCE SCREEN CHANGE FOR PFKEYS               
*                                                                               
INIT10   OI    SI2LN1H+6,X'81'                                                  
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
         XC    BSTA,BSTA                                                        
         MVI   BEST,0                                                           
*                                  CLR NAME FIELDS                              
***************                                                                 
* MEDIA FIELD                                                                   
***************                                                                 
VK05     DS    0H                                                               
         LA    R2,SCOMEDH          MEDIA                                        
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    SCOCLTH+4,X'DF'     FORCE VALIDATION OF NEXT FLD                 
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLI   8(R2),C'C'                                                       
         BE    INVERR                                                           
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
***************                                                                 
* CLIENT FIELD                                                                  
***************                                                                 
VK10     DS    0H                                                               
         LA    R2,SCOCLTH          CLIENT FIELD                                 
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         NI    SCOPRDH+4,X'DF'     FORCE VALIDATION OF NEXT FLD                 
         XC    BCLT,BCLT                                                        
*                                                                               
VK15     DS    0H                                                               
         CLI   5(R2),0             ASSUME 'ALL' IF NO INPUT                     
         BNE   VK17                                                             
         MVC   8(3,R2),=C'ALL'                                                  
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         B     VK20                                                             
*                                                                               
VK17     DS    0H                                                               
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK18                                                             
         B     VK20                                                             
*                                                                               
VK18     DS    0H                                                               
         BAS   RE,TSTOFF           TEST IF OFFICE CODE                          
         BE    VK50                DON'T SET VALID                              
         GOTO1 VALICLT                                                          
*                                                                               
VK20     DS    0H                                                               
         OI    4(R2),X'20'                                                      
*                                                                               
***************                                                                 
* PRODUCT FIELD                                                                 
***************                                                                 
VK20A    DS    0H                                                               
         LA    R2,SCOPRDH          PRODUCT FIELD                                
*        TM    4(R2),X'20'                                                      
*        BO    VK30                                                             
         NI    SCOESTH+4,X'DF'     FORCE VALIDATION OF NEXT FLD                 
*                                                                               
VK22     DS    0H                                                               
         XC    MYPRD,MYPRD                                                      
         CLI   5(R2),0                                                          
         BNE   VK23                                                             
         CLC   =C'ALL',SCOCLT      IF NOT ALL CLIENTS AND NO PRODUCT            
         BE    VK30                                                             
         MVC   8(3,R2),=C'ALL'     THEN ASSUME 'ALL' PRODUCTS                   
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         B     VK30                                                             
*                                                                               
VK23     DS    0H                                                               
         CLC   =C'ALL',SCOCLT      IF ALL CLIENTS                               
         BE    INVERR              THEN NOTHING AFTER IS ALLOWED                
         CLC   =C'ALL',8(R2)                                                    
         BNE   VK24                                                             
         B     VK30                                                             
*                                                                               
VK24     DS    0H                                                               
         CLC   8(4,R2),=C'PGR='                                                 
         BNE   VK25                                                             
         B     VK30                                                             
*                                                                               
VK25     DS    0H                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
VK27     DS    0H                                                               
*                                                                               
VK28     DS    0H                                                               
         MVC   MYPRD,QPRD          CHARACTER PRD CODE FOR I2COM REC             
*                                                                               
VK30     DS    0H                                                               
         OI    4(R2),X'20'                                                      
*                                                                               
***************                                                                 
* ESTIMATE FIELD                                                                
***************                                                                 
         LA    R2,SCOESTH          ESTIMATE FIELD                               
         TM    4(R2),X'20'                                                      
         BO    VK40                                                             
*                                                                               
VK35     DS    0H                                                               
         MVI   BEST,0                                                           
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
VK37     DS    0H                                                               
         CLC   =C'ALL',SCOCLT      IF ALL CLIENTS                               
         BE    INVERR              THEN NOTHING AFTER IS ALLOWED                
*                                                                               
         CLC   =C'ALL',8(R2)       IF ALL ESTIMATES                             
         BNE   VK37A                                                            
         B     VK40                NOTHING TO VALIDATE                          
*                                                                               
VK37A    DS    0H                                                               
         CLC   =C'ALL',SCOPRD      IF PRODUCT SPECIFIED                         
         BE    *+14                                                             
         CLC   =C'PGR=',SCOPRD        AND NOT PGROUP                            
         BNE   VK38                THEN VALIDATE THE ESTIMATE                   
         TM    4(R2),X'08'         WE HAVE TO VALIDATE OURSELVES                
         BZ    INVERR                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'255'          ESTIMATE 1-255                               
         BH    INVERR                                                           
         LTR   R1,R1                                                            
         BZ    INVERR                                                           
         STC   R1,BEST                                                          
         B     VK40                                                             
*                                                                               
VK38     DS    0H                                                               
         LA    R2,SCOESTH          ESTIMATE FIELD                               
         GOTO1 VALIEST                                                          
*                                                                               
VK40     DS    0H                                                               
         OI    4(R2),X'20'                                                      
*                                                                               
***************                                                                 
* MARKET FIELD                                                                  
***************                                                                 
VK40A    DS    0H                                                               
*                                                                               
VK45     DS    0H                                                               
*                                                                               
VK46     DS    0H                                                               
         LA    R2,SI2STATH                                                      
         TM    4(R2),X'20'                                                      
         BO    VK50                                                             
         XC    BSTA,BSTA                                                        
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VK50                                                             
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BO    VK49                IT IS A MARKET INPUT                         
*                                                                               
VK47     DS    0H                                                               
         GOTO1 VALINTWK                                                         
         B     VK50                                                             
*                                                                               
VK49     DS    0H                                                               
         LA    R2,SI2STATH         HANDLING SCOM'S MKT                          
         GOTO1 VALIMKT                                                          
         MVC   SCMMKTN,MKTNM                                                    
         OI    SCMMKTNH+6,X'80'    DISPLAY MARKET NAME ON SCOM SCREEN           
*                                                                               
VK50     DS    0H                                                               
         LA    R6,KEY              SET UP KEY                                   
         USING COMHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   COMKTYPE,=X'0D0C'                                                
         MVC   COMKAGY,BAGYMD                                                   
         MVC   COMCTYPE,CONREC     RECORD TYPE                                  
*                                                                               
VK60     DS    0H                                                               
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
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
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
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'255'          ESTIMATE 1-255                               
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
VK80     DS    0H                                                               
         MVC   SVRECNUM,TWALREC                                                 
*                                                                               
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '  I2COM RECORDS LIVES ON XFILE                
         MVC   SYSDIR,=C'XSPDIR  '                                              
*&&DO                                                                           
         GOTO1 HIGH                                                             
*&&                                                                             
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(32),KEY                                                  
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        TEST FOR OFFICE CODE                                                   
*        R2 A(CLIENT FLD)                                                       
*                                                                               
TSTOFF   NTR1                                                                   
         CLI   5(R2),2                                                          
         BNE   TONO                                                             
         CLI   8(R2),C'*'                                                       
         BNE   TONO                                                             
         CLI   9(R2),C'A'                                                       
         BL    INVERR                                                           
         CLI   9(R2),C'Z'                                                       
         BNH   TO10                                                             
         CLI   9(R2),C'0'                                                       
         BL    INVERR                                                           
         CLI   9(R2),C'9'                                                       
         BH    INVERR                                                           
*                                                                               
TO10     CLI   CONREC,C'M'        OFFICE NOT ALLOWED IN MCOM                    
         BE    INVERR                                                           
         CLI   CONREC,C'P'        OFFICE NOT ALLOWED IN PCOM                    
         BE    INVERR                                                           
         CLC   CONREC(2),=C'SC'   OFFICE NOT ALLOWED IN SCOM                    
         BE    INVERR                                                           
         MVC   BCLT,8(R2)         OFFICE CODE IS VALID                          
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
         ZIC   R1,HALF+1                                                        
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
         ZIC   R5,5(R2)                                                         
         SH    R5,=H'4'            SUBTRACT PGR=                                
         BZ    GGX                                                              
         CH    R5,=H'4'            MAX OF 4 CHARS                               
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
RPX      MVC   AIO,AIO2                                                         
         B     EXIT                EXIT WITH CC SET                             
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         MVC   LKEY,=H'32'                                                      
         MVC   DATADISP,=H'42'     POINTS TO FIRST ELEMENT                      
         MVC   SIZEIO,=F'3975'                                                  
         MVC   SYSFIL,=C'XSPFIL'   I2COM RECORDS LIVES ON XFILE                 
         MVC   SYSDIR,=C'XSPDIR'                                                
         MVC   AIO,AIO2                                                         
*                                                                               
* MAKE SURE RECORD IS IN AIO2                                                   
*                                                                               
         CLI   ACTNUM,ACTSEL       CHA FROM LIST?                               
         BNE   VR01                                                             
         L     R0,AIO2                                                          
         LA    R1,2000                                                          
         L     RE,AIO1             RECORD IS IN AIO1                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
VR01     DS    0H                                                               
         L     RF,AIO                                                           
         MVC   0(32,RF),KEY                                                     
*                                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BNE   VR02                                                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
VR02     LA    R7,MAXLNS           MAX NUMBER OF LINES                          
*                                                                               
         CLI   CONREC,C'M'         MCOM ONLY HAS 10 LINES                       
         BNE   *+8                                                              
         LA    R7,MAXMCLNS                                                      
         CLI   CONREC,C'P'         PCOM ONLY HAS 10 LINES                       
         BNE   *+8                                                              
         LA    R7,MAXMCLNS                                                      
         CLC   CONREC(2),=C'SC'    SCOM ONLY HAS 10 LINES                       
         BNE   *+8                                                              
         LA    R7,MAXMCLNS                                                      
*                                                                               
         STC   R7,MXLINES                                                       
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
         ZIC   R1,1(R6)            BUMP PAST 01 ELEMENT                         
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
         ZIC   R7,MXLINES          MAX NUMBER OF LINES                          
         MVI   ELCODE,X'05'        SET FOR TOP LINES                            
*                                                                               
VR10     CLI   5(R2),0             ANY INPUT ON THIS LINE                       
         BE    VR30                                                             
         BAS   RE,CHKTB            CHECK IF TOP/BOTTOM                          
         BE    VR50                                                             
*                                                                               
VR20     XC    ELEM,ELEM                                                        
         ZIC   R1,5(R2)            GET LENGTH OF INPUT                          
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
         ZIC   R1,1(R6)            ELSE BUMP PAST CURRENT ELEMENT               
         AR    R6,R1                                                            
*                                                                               
VR45     MVC   ELEM(1),ELCODE                                                   
         CLI   CONREC,C'I'                                                      
         BE    VR47                                                             
         GOTO1 =V(RECUP),DMCB,(0,AIO),ELEM,(R6)                                 
         B     VR48                                                             
VR47     DS    0H                                                               
*!!!!    GOTO1 =V(RECUP),DMCB,(X'FE',AIO),ELEM,(R6),RECSPC                      
         GOTO1 HELLO,DMCB,(C'P',=C'XSPFIL '),(X'05',AIO),ELEM,         *        
               =C'ADD=END'                                                      
*                                                                               
VR48     ZIC   R1,ELSADDED                                                      
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
         GOTO1 =V(RECUP),DMCB,(0,AIO),(R6),(R6)                                 
         B     CP60                                                             
*                                                                               
CP25     DS    0H                                                               
*!!!!    GOTO1 =V(RECUP),DMCB,(X'FE',AIO),(R6),(R6),RECSPC                      
         MVI   0(R6),X'FF'                                                      
         GOTO1 HELLO,DMCB,(C'D',=C'XSPFIL '),(X'FF',AIO),(R6),         *        
               =C'ADD=END'                                                      
         B     CP60                                                             
*                                                                               
CP50     ZIC   R1,1(R6)            GET START OF NEXT ELEMENT                    
         AR    R6,R1                                                            
         MVC   ELEM,ELCODE                                                      
         MVC   ELEM+1(2),=X'0300'                                               
         CLI   CONREC,C'I'                                                      
         BNE   CP55                                                             
*!!!     GOTO1 =V(RECUP),DMCB,(X'FE',AIO),ELEM,(R6),RECSPC                      
         GOTO1 HELLO,DMCB,(C'P',=C'XSPFIL '),(ELCODE,AIO),ELEM,        *        
               =C'ADD=END'                                                      
         B     CP57                                                             
CP55     GOTO1 =V(RECUP),DMCB,(0,AIO),ELEM,(R6)                                 
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
         ZIC   R7,MXLINES          MAX NUMBER OF LINES                          
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
         ZIC   R4,NUMELS                                                        
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
DR       DS    0H                                                               
         MVC   AIO,AIO2                                                         
*                                                                               
         CLC   CONREC(2),=C'SC'                                                 
         BNE   DR00                                                             
         TWAXC SCMLN1H                                                          
         B     DR00B                                                            
DR00     CLI   CONREC,C'I'         I2COM RECS?                                  
         BNE   DR00A                                                            
         TWAXC SI2LN1H                                                          
*                                                                               
         CLI   ACTNUM,ACTCHA                                                    
         BE    DR00B                                                            
         CLI   ACTNUM,ACTADD                                                    
         BE    DR00B                                                            
         CLI   ACTNUM,ACTSEL                                                    
         BE    DR00B                                                            
         GOTO1 GETREC                                                           
*                                                                               
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
         ZIC   R7,MXLINES          LINE COUNTER                                 
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
DR30     DS    0H                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
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
PO20     ZIC   R1,1(R6)            ELEMENT LENGTH                               
         SH    R1,=H'3'            SUB ELCODE/ELEM LENGTH/& 1 FOR EX            
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
NXTUN    ZIC   RF,0(R2)                                                         
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
*                                                                               
* MAKE SURE RECORD IS IN AIO2                                                   
*                                                                               
         L     R0,AIO2                                                          
         LA    R1,2000                                                          
         L     RE,AIO1             RECORD IS IN AIO1                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
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
DKI02    ZIC   RE,COMI2KAM                                                      
         SLL   RE,28                                                            
         SRL   RE,28                                                            
         IC    RE,MEDTAB-1(RE)                                                  
         STC   RE,SI2MED                                                        
*                                                                               
         OC    COMI2KCL,COMI2KCL                                                
         BNZ   DKI03                                                            
         MVC   SI2CLT(3),=C'ALL'                                                
         B     DKI05                                                            
DKI03    GOTO1 CLUNPK,DMCB,COMI2KCL,SI2CLT                                      
*                                                                               
*KI05    MVI   SI2PRD,C' '                                                      
*        MVC   SI2PRD+1(L'SI2PRD-1),SI2PRD                                      
*                                                                               
DKI05    OC    COMI2KPR,COMI2KPR                                                
         BNZ   DKI07                                                            
*                                                                               
         CLC   =C'ALL',SI2CLT                                                   
         BE    DKI10                                                            
         MVC   SI2PRD(3),=C'ALL'                                                
         B     DKI10                                                            
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
DKI15    ZIC   R0,WKI2KEST                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SI2EST,DUB                                                       
*                                                                               
DKI20    DS    0H                                                               
         OC    WKI2KST,WKI2KST                                                  
         BZ    DKI40                                                            
         XC    WORK,WORK                                                        
         MVC   WORK+12(3),WKI2KST                                               
         GOTO1 VMSUNPK,DMCB,WORK+10,DUB,WORK                                    
*                                                                               
DKI30    MVC   SI2STAT(4),WORK                                                  
*&&DO                                                                           
         CLI   WORK+4,C' '                                                      
         BE    DKI40                                                            
         CLI   WORK+4,C'/'                                                      
         BNE   *+14                                                             
         MVC   SI2STAT+4(1),WORK+4                                              
         B     DKI40                                                            
         LA    R1,SI2STAT+3                                                     
         CLI   0(R1),C' '                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'-'                                                       
         MVC   2(1,R1),WORK+4                                                   
*&&                                                                             
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
         ZIC   R0,COMI2KE2                                                      
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
*                                                                               
DKX40    DS    0H                                                               
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
*&&DO                                                                           
* CODE REMOVED TO SUPPORT CLIST2 HWON                                           
*                                                                               
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
* CODE REMOVED TO SUPPORT CLIST2 HWON                                           
*&&                                                                             
         GOTO1 GETCLT,DMCB,COMKCLT                                              
         LA    R1,SVCLIST                                                       
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
         MVC   AIO,AIO2                                                         
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
         ZIC   R1,SVBKLNS                                                       
         LA    R1,WORK+5(R1)                                                    
         XC    0(3,R1),0(R1)                                                    
         MVC   KEY(13),SVKEY       RESTORE DIR FOR SEQ READING                  
         GOTO1 HIGH                                                             
         MVC   AIO,AIO2                                                         
*                                                                               
GPX      B     EXIT                                                             
         EJECT                                                                  
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
         CLC   SVKEY+6,=X'000000'  PRODUCT OR GROUP SPECIFIED                   
         BE    REQ20                                                            
         CLI   SVKEY+6,X'00'       PRODUCT NUMBER USED                          
         BE    REQ10                                                            
         MVC   8(1,R1),SVKEY+6                                                  
*                                                                               
         ZIC   R3,SVBKLNS                                                       
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
         ZIC   R3,SVKEY+9          YES                                          
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
         B     TRAPEND                                                          
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPEND                                                          
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
       ++INCLUDE NESFMWORKD                                                     
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
SAVEKEY  DS    XL48                                                             
*                                                                               
*                                                                               
         PRINT OFF                                                              
         SPACE 1                                                                
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'208NESFM45   10/31/05'                                      
         END                                                                    

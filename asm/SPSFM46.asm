*          DATA SET SPSFM46    AT LEVEL 099 AS OF 04/26/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE T21746A                                                                  
*                                                                               
         TITLE 'SPSFM46 COMMENT LIST'                                           
T21746   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21746*                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,SETFILE                                                     
         BE    SF                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   MAIN10                                                           
         LA    R2,LISTAR                                                        
         B     LR                                                               
*                                                                               
MAIN10   CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   EXIT                                                             
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,MYHOOK                                                        
         ST    R1,HEADHOOK                                                      
         LA    R2,P                                                             
         B     LR                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE KEY ROUTINE                                                   
*                                                                               
VK       DS    0H                                                               
         BAS   RE,XSF                                                           
         CLC   SVRECNUM,TWALREC    HAS RECORD TYPE CHANGED                      
         BE    VK00                                                             
         XC    MYCLT,MYCLT                                                      
         XC    MYPRD,MYPRD                                                      
         XC    MYEST,MYEST                                                      
         XC    MYSTA,MYSTA                                                      
*                                                                               
VK00     CLC   CONREC(2),=C'SC'    SCOM REC?                                    
         BNE   VK01                                                             
         CLI   LSCSTATH+5,0                                                     
         BE    *+12                                                             
         TM    LSCSTATH+4,X'08'    VALID NUMBERIC?                              
         BO    *+14                                                             
         XC    LSCMKTN,LSCMKTN     CLR MKT NAME FROM SCREEN                     
         OI    LSCMKTNH+6,X'80'                                                 
*                                                                               
VK01     LA    R2,LCOMEDIH                                                      
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         CLI   CONREC,C'N'         NVTEXT - MEDIA 'C' INVALID                   
         BNE   VK05                                                             
         CLI   QMED,C'C'                                                        
         BE    INVERR                                                           
*                                                                               
VK05     LA    R2,LCOCLTH                                                       
         XC    MYCLT,MYCLT                                                      
         CLI   5(R2),0                                                          
         BE    VK40                                                             
VK07     BAS   RE,TSTOFF           TEST FOR OFFICE CODE                         
         BE    VK40                                                             
         GOTO1 VALICLT                                                          
         MVC   MYCLT,BCLT                                                       
*                                                                               
VK10     CLI   CONREC,C'N'         NVTEXT                                       
         BE    VK40                                                             
         CLC   CONREC(2),=C'AC'    ACOM?                                        
         BE    VK40                YES                                          
         LA    R2,LCOPRDH                                                       
         XC    MYPRD,MYPRD                                                      
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         CLC   8(4,R2),=C'PGR='                                                 
         BNE   VK15                                                             
         BAS   RE,CHKPGR           PRODUCT GROUP                                
         B     VK40                                                             
*                                                                               
VK15     GOTO1 VALIPRD                                                          
         CLI   CONREC,C'I'                                                      
         BE    VK18                                                             
         MVC   MYPRD+2(L'BPRD),BPRD                                             
         B     VK20                                                             
*                                                                               
VK18     CLC   =C'ALL',QPRD                                                     
         BNE   VK19                                                             
         MVC   MYPRD,=3X'00'                                                    
         B     VK20                                                             
VK19     MVC   MYPRD,QPRD          I2COM TAKES CHARACTER PRD IN KEY             
*                                                                               
VK20     LA    R2,LCOESTH                                                       
         MVI   MYEST,0                                                          
         CLC   =C'ALL',LCOEST      ACCEPT ALL FOR ESTIMATE FIELD                
         BNE   *+12                                                             
         MVI   MYEST,X'00'                                                      
         B     VK30                                                             
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         BZ    INVERR                                                           
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    INVERR              NOPE                                         
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         CHI   RE,1                TEST IN RANGE 1-255                          
         BL    INVERR                                                           
         CHI   RE,255                                                           
         BH    INVERR                                                           
         STC   RE,MYEST            SET BINARY ESTIMATE                          
*                                                                               
VK30     CLC   CONREC(2),=C'SD'    SDR RECS                                     
         BE    VK32                                                             
         CLI   CONREC,C'I'         I2COM RECS                                   
         BE    VK32                                                             
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   VK35                                                             
*                                                                               
         XC    MYSTA,MYSTA                                                      
         CLI   LSCSTATH+5,0        MARKET/STATION TO VALIDATE?                  
         BE    VK40                                                             
         CLC   LSCSTAT(3),=C'ALL'                                               
         BE    VK40                                                             
         TM    LSCSTATH+4,X'08'    MARKET INPUT?                                
         BNO   VK32                                                             
         XC    MYSTA,MYSTA                                                      
         LA    R2,LSCSTATH                                                      
         GOTO1 VALIMKT                                                          
         MVC   MYSTA+1(L'BMKT),BMKT                                             
         MVC   LSCMKTN,MKTNM                                                    
         OI    LSCMKTNH+6,X'80'                                                 
         B     VK40                                                             
*                                                                               
VK32     LA    R2,LSDSTATH                                                      
         XC    MYSTA,MYSTA                                                      
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 VALISTA                                                          
         MVC   MYSTA,BMKTSTA+2                                                  
*                                                                               
VK35     CLI   CONREC,C'M'          MCOM RECS                                   
         BE    *+12                                                             
         CLI   CONREC,C'P'          AND PCOMS                                   
         BNE   VK40                                                             
         LA    R2,LMCMKTH                                                       
         XC    MYSTA,MYSTA                                                      
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 VALIMKT                                                          
         MVC   MYSTA+1(L'BMKT),BMKT                                             
*                                                                               
         USING COMHDRD,R6                                                       
VK40     XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   COMKTYPE,=X'0D0C'   RECORD TYPE                                  
         MVC   COMKAGY,BAGYMD      AGY/MEDIA                                    
         MVC   COMCTYPE,CONREC     RECORD TYPE                                  
         CLC   CONREC(2),=C'AC'                                                 
         BNE   *+12                                                             
         MVI   COMCTYPE,C'1'       "1" IS ASSIGNED TO ACOM RECS                 
         B     VK50                                                             
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   *+12                                                             
         MVI   COMCTYPE,C'T'       "T" IS ASSIGNED TO SCOM RECS                 
         B     VK50                                                             
         CLC   =C'A3',CONREC                                                    
         BNE   *+8                                                              
         MVI   COMCTYPE,C'3'                                                    
         CLI   CONREC,C'B'         B4-B7 COMMENTS TYPES                         
         BNE   VK50                                                             
         CLC   =C'BC',CONREC                                                    
         BE    VK50                                                             
         MVC   COMCTYPE,CONREC+1                                                
*                                                                               
VK50     CLI   CONREC,C'I'         I2COM RECORD NEEDS MORE VALIDATION           
         BNE   VK60                                                             
*********************************************************************           
*        I2COM HAS TO VALIDATE ALSO MON/YR, PRD2, EST2              *           
*********************************************************************           
*                                                                               
         MVC   MYEST,BEST                                                       
         MVC   MYPRD1,MYPRD                                                     
         MVC   MYAMD,BAGYMD                                                     
*                                                                               
         LA    R2,LI2MYRH                                                       
         XC    MYDATE,MYDATE                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BE    VI10                                                             
         GOTO1 DATVAL,DMCB,(2,8(R2)),MYDATE                                     
         CLC   =C'000000',MYDATE                                                
         BE    INVERR                                                           
*                                                                               
VI10     LA    R2,LI2PR2H          PRODUCT 2 FIELD                              
*                                                                               
VI15     XC    MYPRD,MYPRD                                                      
         CLI   5(R2),0                                                          
         BNE   VI25                                                             
         MVC   LI2ES2,=X'404040'                                                
         OI    LI2ES2H+6,X'80'                                                  
         B     VI65                                                             
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
VI40     LA    R2,LI2ES2H          ESTIMATE 2 FIELD                             
*                                                                               
VI50     MVI   BEST,0                                                           
         CLI   5(R2),0                                                          
         BNE   VI55                                                             
         CLC   =C'ALL',LCOCLT      IF NOT ALL CLIENTS                           
         BE    VI65                                                             
         CLC   =C'ALL',LI2PR2      OR NOT ALL PRODUCTS                          
         BE    VI65                                                             
         MVC   8(3,R2),=C'ALL'     THEN ASSUME 'ALL' ESTIMATE                   
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         B     VI65                                                             
*                                                                               
VI55     CLC   =C'ALL',LCOCLT      IF ALL CLIENTS                               
         BE    INVERR              THEN NOTHING AFTER IS ALLOWED                
*                                                                               
         CLC   =C'ALL',8(R2)       IF ALL ESTIMATES                             
         BE    VI65                                                             
*                                                                               
VI55A    CLC   =C'ALL',LI2PR2      IF PRODUCT SPECIFIED                         
         BE    *+14                                                             
         CLC   =C'PGR=',LI2PR2     AND NOT PGROUP                               
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
         MVC   COMI2KAM,BAGYMD                                                  
         MVI   COMI2KTY,C'I'                                                    
         MVC   COMI2KCL,MYCLT                                                   
         MVC   COMI2KPR,MYPRD1                                                  
         MVC   COMI2KES,MYEST                                                   
         MVC   COMI2KP2,MYPRD                                                   
         MVC   COMI2KE2,BEST                                                    
         MVC   COMI2KST,BSTA                                                    
         CLC   MYDATE,=6X'0'                                                    
         BE    VK70                                                             
         GOTO1 DATCON,DMCB,(0,MYDATE),(3,COMI2KYM)                              
         B     VK70                DONE WITH VALKEY                             
*********************************************************************           
VK60     MVC   COMKCLT,MYCLT       CLIENT  FILTER                               
         MVC   COMKPRD,MYPRD       PRODUCT START AT                             
         MVC   COMKEST,MYEST       ESTIMATE START AT                            
         MVC   COMKSTA,MYSTA       STATION/MARKET                               
*                                                                               
VK70     MVC   MYSVKEY,KEY                                                      
         MVC   SVRECNUM,TWALREC                                                 
         B     EXIT                                                             
         EJECT                                                                  
         DROP  R6                                                               
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
TO10     MVC   MYCLT(1),8(R2)        OFFICE CODE IS VALID                       
         MVC   MYCLT+1(1),O.OFCOFC                                              
         XC    MYPRD,MYPRD                                                      
         MVI   MYEST,0                                                          
         XC    MYSTA,MYSTA                                                      
*                                                                               
         LA    R2,LCOPRDH                                                       
         CLI   5(R2),0                                                          
         BNE   INVERR              NO MORE FIELDS ALLOWED                       
         LA    R2,LCOESTH                                                       
         CLI   5(R2),0                                                          
         BNE   INVERR                                                           
         B     YES                                                              
*                                                                               
TONO     B     NO                                                               
         DROP  O                                                                
         EJECT                                                                  
*                                                                               
*        CHECK PRODUCT GROUP                                                    
*                                                                               
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
CPG10    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),BAGYMD         A/M                                      
         MVC   KEY+3(2),MYCLT          CLT                                      
         MVC   KEY+5(3),MYPRD          PGRP                                     
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE      TY/A-M/CLT/PGRPID                            
         BNE   INVERR                                                           
*                                                                               
         CLI   HALF+1,0            TEST ADDING DEFAULT                          
         BE    CPGX                                                             
         CLC   KEY(13),KEYSAVE      TY/A-M/CLT/PGRPID                           
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
         CHI   R5,4                MAX OF 3 CHARS                               
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
         MVC   KEY+3(2),MYCLT                                                   
         MVC   KEY+5(1),FULL                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVERR                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING PRGRECD,R4                                                       
         LA    R6,PRGEL                                                         
         USING PRGEL01,R6                                                       
         CLC   PRGBK1LN(1),HALF+1                                               
         BNE   INVERR                                                           
*                                                                               
RPX      MVC   AIO,AIO1                                                         
         B     EXIT                EXIT WITH CC SET                             
         EJECT                                                                  
         DROP  R4                                                               
         DROP  R6                                                               
*                                                                               
*        SET XFILE FOR I2COM RECS                                               
*                                                                               
SETFIL   NTR1                                                                   
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
XSF      NTR1                                                                   
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SIZEIO,=A(LIOS)                                                  
         MVC   SYSFIL,=C'SPTFIL'                                                
         MVC   SYSDIR,=C'SPTDIR'                                                
XSFX     B     EXIT                                                             
*                                                                               
*        LIST RECORDS ROUTINE                                                   
*                                                                               
         USING LISTD,R2                                                         
LR       DS    0H                                                               
         CLI   CONREC,C'I'         SEPARATE CODES FOR I2COM RECS                
         BE    LRI                                                              
         OC    KEY(L'COMKEY),KEY   FIRST TIME THROUGH                           
         BNZ   *+10                NO, READ WHERE LEFT OFF                      
         MVC   KEY,MYSVKEY         RECALL WHAT WE'RE LOOKING FOR                
         GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR10     GOTO1 SEQ                                                              
*                                                                               
LR20     LA    R6,KEY                                                           
         USING COMHDRD,R6                                                       
         CLC   KEY(COMKCLT-COMHDRD),MYSVKEY  COMP UP TO CLIENT                  
         BNE   LRX                                                              
         CLI   CONREC,C'N'         NVTEXT - CLIENT IS START AT                  
         BE    LR30                NOT FILTER                                   
         CLC   CONREC(2),=C'AC'    ACOM - CLIENT IS START AT                    
         BE    LR30                NOT FILTER                                   
         OC    MYCLT,MYCLT         IS THERE A CLIENT FILTER                     
         BZ    LR30                                                             
         CLC   COMKCLT,MYCLT                                                    
         BNE   LRX                 DONE - WE READ HIGH FOR IT                   
*                                                                               
LR30     OC    COMKCLT,COMKCLT                                                  
         BZ    LR40                                                             
         TM    COMKCLT,X'80'       TEST THIS IS AN OFFICE                       
         BZ    LR32                                                             
         BAS   RE,GETAAN                                                        
         GOTO1 CLUNPK,DMCB,(CLTAAN,COMKCLT),LCLT                                
         B     LR33                                                             
*                                                                               
O        USING OFFICED,WORK                                                     
LR32     XC    WORK,WORK                                                        
         MVI   O.OFCSYS,C'S'                                                    
         MVC   O.OFCAGY,AGENCY                                                  
         MVC   O.OFCOFC,COMKCLT+1                                               
         GOTO1 OFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                            
         MVI   LCLT,C'*'                                                        
         MVC   LCLT+1(2),O.OFCOFC2                                              
         DROP  O                                                                
*                                                                               
LR33     OC    COMKPRD,COMKPRD     PRODUCT                                      
         BZ    LR35                                                             
         BAS   RE,GETPRD                                                        
         MVC   LPRD,WORK                                                        
*                                                                               
LR35     SR    R0,R0               ESTIMATE                                     
         ICM   R0,1,COMKEST                                                     
         BZ    LR40                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST,DUB                                                         
*                                                                               
         OC    COMKSTA,COMKSTA     MCOM/SDR RECORDS                             
         BZ    LR40                                                             
         CLI   CONREC,C'B'                                                      
         BE    LR36                                                             
         CLI   CONREC,C'M'                                                      
         BE    LR36                                                             
         CLI   CONREC,C'P'                                                      
         BE    LR36                                                             
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   LR37                                                             
         CLI   COMKSTA,X'00'       IS IT MKT?                                   
         BNE   LR37                                                             
*                                                                               
LR36     SR    R1,R1                                                            
         ICM   R1,3,COMKSTA+1                                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         CLC   CONREC(2),=C'SC'    SCOM RECS?                                   
         BNE   *+20                                                             
         UNPK  LSTA+3(4),DUB                                                    
         MVC   LSTA(3),=C'MKT'                                                  
         B     *+10                                                             
         UNPK  LSTA(4),DUB                                                      
         B     LR40                                                             
*                                                                               
LR37     XC    WORK,WORK                                                        
         MVC   WORK+12(3),COMKSTA                                               
         GOTO1 MSUNPK,DMCB,(X'80',WORK+10),DUB,WORK                             
         MVC   LSTA,WORK                                                        
         CLI   COMKSTA,X'F0'       HAVE A SYSCODE?                              
         BL    *+12                NO                                           
         MVI   LSTA+4,C'/'         YES - MOVE IN A /                            
         B     LR40                                                             
*                                                                               
         CLI   WORK+4,C' '         DOING RADIO?                                 
         BE    LR40                NO                                           
         LA    R1,LSTA+3                                                        
         CLI   0(R1),C' '                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'-'          KEVIN                                        
         MVC   2(1,R1),WORK+4                                                   
*                                                                               
LR40     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    LR50                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR70                                                             
*                                                                               
LR50     MVC   LCOM,SPACES                                                      
         LLC   RE,1(R6)            GET ELEMENT LENGTH                           
         SHI   RE,3                                                             
         CHI   RE,44               TRYING TO MOVE IN MORE PAST SCREEN?          
         BNH   *+8                 NO, ITS OK                                   
         LA    RE,44               DONT CLOBBER GENCON FIELDS!                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LCOM(0),2(R6)                                                    
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR60                                                             
         GOTO1 LISTMON                                                          
         MVC   LISTAR,SPACES                                                    
         B     LR70                                                             
*                                                                               
LR60     GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LR70     B     LR10                                                             
*                                                                               
LRX      B     EXIT                                                             
**************************************************************                  
* I2COM GETS ITS OWN LIST CODES, TOO COMPLICATED TO MERGE    *                  
**************************************************************                  
         USING I2LSD,R2                                                         
LRI      DS    0H                                                               
         OC    KEY(L'XCOMKEY),KEY  FIRST TIME THROUGH                           
         BNZ   *+10                                                             
         MVC   KEY,MYSVKEY                                                      
         GOTO1 HIGH                NO, READ WHERE LEFT OFF                      
         B     LRI20                                                            
*                                                                               
LRI10    GOTO1 SEQ                                                              
*                                                                               
LRI20    LA    R6,KEY                                                           
         USING COMI2HD,R6                                                       
         CLC   KEY(COMI2KCL-COMI2K),MYSVKEY                                     
         BNE   LRIX                                                             
         OC    MYCLT,MYCLT                                                      
         BZ    LRI30                                                            
         CLC   COMI2KCL,MYCLT                                                   
         BNE   LRIX                                                             
*                                                                               
LRI30    OC    COMI2KCL,COMI2KCL                                                
         BZ    LRI40                                                            
         TM    COMI2KCL,X'80'       TEST THIS IS AN OFFICE                      
         BZ    LRI32                                                            
         BAS   RE,GETAAN                                                        
         GOTO1 CLUNPK,DMCB,(CLTAAN,COMI2KCL),I2CLT                              
         B     LRI33                                                            
*                                                                               
O        USING OFFICED,WORK                                                     
LRI32    XC    WORK,WORK                                                        
         MVI   O.OFCSYS,C'S'                                                    
         MVC   O.OFCAGY,AGENCY                                                  
         MVC   O.OFCOFC,COMI2KCL+1                                              
         GOTO1 OFFICER,DMCB,(C'2',WORK),(0,ACOMFACS)                            
         MVI   I2CLT,C'*'                                                       
         MVC   I2CLT+1(2),O.OFCOFC2                                             
         DROP  O                                                                
*                                                                               
*                                                                               
LRI33    OC    COMI2KPR,COMI2KPR                                                
         BZ    LRI35                                                            
         MVC   I2PRD,COMI2KPR       I2COM HAS EBCDIC PRD CODE                   
*                                                                               
LRI35    SR    R0,R0                                                            
         ICM   R0,1,COMI2KES                                                    
         BZ    LRI37                                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  I2EST,DUB                                                        
*                                                                               
LRI37    OC    COMI2KST,COMI2KST                                                
         BZ    LRI40                                                            
         XC    WORK,WORK                                                        
         MVC   WORK+12(3),COMI2KST                                              
         GOTO1 MSUNPK,DMCB,WORK+10,DUB,WORK                                     
         MVC   I2STA(4),WORK                                                    
         CLI   WORK+4,C' '                                                      
         BE    LRI40                                                            
         CLI   WORK+4,C'/'                                                      
         BNE   *+14                                                             
         MVC   I2STA+4(1),WORK+4                                                
         B     LRI40                                                            
*                                                                               
         LA    R1,I2STA+3                                                       
         CLI   0(R1),C' '                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'-'                                                       
         MVC   2(1,R1),WORK+4                                                   
*                                                                               
LRI40    OC    COMI2KP2,COMI2KP2                                                
         BZ    LRI45                                                            
         MVC   I2PR2,COMI2KP2                                                   
*                                                                               
LRI45    SR    R0,R0                                                            
         ICM   R0,1,COMI2KE2                                                    
         BZ    LRI50                                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  I2ES2,DUB                                                        
*                                                                               
LRI50    OC    COMI2KYM,COMI2KYM                                                
         BZ    LRI55                                                            
         GOTO1 DATCON,DMCB,(3,COMI2KYM),(6,I2MYR)                               
*                                                                               
LRI55    DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    LRI60                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRI70                                                            
*                                                                               
LRI60    MVC   I2COM,SPACES                                                     
         LLC   RE,1(R6)                                                         
         SHI   RE,3                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   I2COM(0),2(R6)                                                   
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LRI65                                                            
         GOTO1 LISTMON                                                          
         MVC   LISTAR,SPACES                                                    
         B     LRI70                                                            
*                                                                               
LRI65    GOTO1 CATCHIOS                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
LRI70    B     LRI10                                                            
*                                                                               
LRIX     B     EXIT                                                             
         EJECT                                                                  
* SUBROUTINE TO TRANSLATE BINARY PRD CODE TO EBCDIC                             
* OR DISPLAY PRODUCT GROUP CODE                                                 
* IF CLIENT HEADER NEEDED, READ IT INTO REC2                                    
         SPACE 1                                                                
GETPRD   NTR1                                                                   
         XC    WORK,WORK           CLEAR OUTPUT DISPLAY AREA                    
         CLI   KEY+6,0             TEST PRDGRP PRESENT                          
         BNE   GP40                YES                                          
*                                  *** PRODUCT CODE ***                         
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         LA    R6,SVKEY                                                         
         USING COMHDRD,R6          USED TO BE R6                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),COMKAGY                                                 
         MVC   KEY+2(3),COMKCLT                                                 
         MVC   AIO,AIO2                                                         
         L     R4,AIO                                                           
         CLC   KEY(13),0(R4)       TEST HAVE CLTHDR ALREADY                     
         BE    GP10                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GP05                                                             
* BETTER NOT TO DIE IF CLIENT HAS BEEN DELETED                                  
         LA    R1,CKEY-CLTHDRD(R4)                                              
         XC    0(256,R1),0(R1)                                                  
         MVC   0(13,R1),KEY        FORCE KEYS EQUAL                             
         LA    R1,CLIST-CLTHDRD(R4)                                             
         XC    0(256,R1),0(R1)     CLEAR THE LIST                               
         B     GP10                                                             
*                                                                               
GP05     GOTO1 GETREC                                                           
*                                                                               
GP10     LA    R1,CLIST-CLTHDRD(R4)    TRANSLATE BINARY CODE *                  
*                                                                               
GP20     CLC   COMKPRD+2(1),3(R1)                                               
         BE    GP30                                                             
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BNL   GP20                                                             
         LA    R1,=C'***'                                                       
*                                                                               
GP30     MVC   WORK(3),0(R1)                                                    
         MVC   AIO,AIO1                                                         
         MVC   KEY(13),SVKEY       RESTORE DIR FOR SEQ READING                  
         GOTO1 HIGH                                                             
         B     GPX                                                              
*                                                                               
GP40     MVC   WORK(1),COMKPRD   DISPLAY PRODUCT GROUP *                        
         UNPK  DUB(5),COMKPRD+1(3)                                              
         MVC   WORK+1(3),DUB                                                    
*                                                                               
GPX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
MYHOOK   NTR1                                                                   
         CLC   CONREC(3),=C'SDR'   SDR RECS                                     
         BNE   MHX                                                              
         MVC   H4+17(8),=C'STATION'                                             
         MVC   H5+17(8),=C'-------'                                             
*                                                                               
MHX      B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
GETAAN   NTR1                                                                   
*                                  *** CPROF+6 (AAN)***                         
         MVI   CLTAAN,C'N'                                                      
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R6,SVKEY                                                         
         USING COMHDRD,R6                                                       
         MVC   KEY+1(1),COMKAGY    AGENCY/MEDIA                                 
         MVC   KEY+2(2),COMKCLT    CLIENT                                       
         CLI   CONREC,C'I'         I2COM RECORD?                                
         BNE   GAAN10              NO                                           
         USING COMI2HD,R6                                                       
         MVC   KEY+1(1),COMI2KAM   YES - AGENCY/MEDIA                           
         MVC   KEY+2(2),COMI2KCL   CLIENT                                       
         DROP  R6                                                               
         BAS   RE,XSF              POINT TO SPOTFILE                            
*                                                                               
GAAN10   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GAAN20                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   CLTAAN,CPROF+6-CLTHDRD(R6)                                       
GAAN20   BAS   RE,SETFIL           RESTORE TO XSPFILE IF NEEDED                 
         MVC   KEY(L'SVKEY),SVKEY  RESTORE DIR FOR SEQ READING                  
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX               NEVER TO RETURN                              
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,60,REQUESTOR                                                  
         SSPEC H2,60,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,34,C'COMMENT LIST'                                            
         SSPEC H2,34,C'------------'                                            
         SPACE 1                                                                
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,8,C'PROD'                                                     
         SSPEC H4,13,C'EST'                                                     
         SSPEC H4,26,C'COMMENT'                                                 
         SPACE 1                                                                
         SSPEC H5,1,C'------'                                                   
         SSPEC H5,8,C'----'                                                     
         SSPEC H5,13,C'---'                                                     
         SSPEC H5,26,C'-------'                                                 
         SPACE 1                                                                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM96D          DSECT FOR RECORD LISTING.                    
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM82D                    SDR LISTING.                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM83D                    MCOM LISTING.                      
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFMF5D                    SCOM LISTING.                      
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM84D                    NVTEXT LISTING.                    
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM62D                    NVTEXT LISTING.                    
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM37D                    ACOM LISTING.                      
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
*                                                                               
         ORG   SYSSPARE                                                         
SVRECNUM DS    XL1                                                              
MYAMD    DS    XL1                                                              
MYCLT    DS    XL2                                                              
MYPRD    DS    XL3                                                              
MYPRD1   DS    XL3                                                              
MYEST    DS    XL1                                                              
MYSTA    DS    XL3                                                              
MYDATE   DS    CL6                                                              
MYSVKEY  DS    CL(L'KEY)                                                        
*                                                                               
CLTAAN   DS    CL1                 SAVED CPROF+6 FROM CLT RECORD                
         EJECT                                                                  
*                                                                               
LISTD    DSECT                                                                  
         DS    CL1                                                              
LCLT     DS    CL3                                                              
         DS    CL3                                                              
LPRD     DS    CL4                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LSTA     DS    CL8                                                              
         DS    CL1                                                              
LCOM     DS    CL45                                                             
*                                                                               
I2LSD    DSECT                                                                  
I2CLT    DS    CL3                                                              
         DS    CL1                                                              
I2PRD    DS    CL3                                                              
         DS    CL1                                                              
I2EST    DS    CL3                                                              
         DS    CL1                                                              
I2PR2    DS    CL3                                                              
         DS    CL1                                                              
I2ES2    DS    CL3                                                              
         DS    CL1                                                              
I2STA    DS    CL5                                                              
         DS    CL1                                                              
I2MYR    DS    CL6                                                              
         DS    CL2                                                              
I2COM    DS    CL40                                                             
         EJECT                                                                  
COMHDRD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
COMI2HD  DSECT                                                                  
       ++INCLUDE SPGENXCOM                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
*DDSPLWORKD                                                                     
*DDSPOOLD                                                                       
*SPGENPRG                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENPRG                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099SPSFM46   04/26/16'                                      
         END                                                                    

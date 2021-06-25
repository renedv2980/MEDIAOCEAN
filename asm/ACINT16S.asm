*          DATA SET ACINT16S   AT LEVEL 002 AS OF 05/01/02                      
*PHASE T61916A,*                                                                
         TITLE 'T61916 - INTERAGENCY PROFILE PEEL REPORT'                       
T61916   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T61916*,R7,CLEAR=YES                                          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         BAS   RE,PREP                                                          
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        VALIDATE THE KEY                                             *         
***********************************************************************         
*                                                                               
VKEY     NTR1                                                                   
         XC    QREC(QLNQ),QREC     CLEAR REQUEST FIELDS                         
*                                                                               
         LA    R2,DELRCVH          VALIDATE RECEIVABLE, IF ENTERED              
         CLI   5(R2),0                                                          
         BE    VKEY01                                                           
         MVC   CUL+1(2),RECVLEDG                                                
         MVI   OPTION,C'Y'                                                      
         GOTO1 VALACCT                                                          
         OI    6(R2),X'80'                                                      
         MVC   QREC,RECCODE        SAVE ACCOUNT                                 
*                                                                               
VKEY01   LA    R2,DELCLTH          CLIENT IS REQUIRED                           
         MVC   CUL+1(2),PRODLEDG                                                
         GOTO1 VALCLI                                                           
         OI    6(R2),X'80'                                                      
         MVC   QCLT,CLICODE        SAVE CLIENT                                  
*                                                                               
VKEY02   LA    R2,DELPRDH          VALIDATE PRODUCT, IF ENTERED                 
         CLI   5(R2),0                                                          
         BE    VKEY04                                                           
         MVI   ERROR,MISSING                                                    
         CLI   DELCLTH+5,0                                                      
         BE    ERREND              MUST HAVE CLIENT IF PRODUCT                  
         GOTO1 VALPROD                                                          
         OI    6(R2),X'80'                                                      
         MVC   QPRD,PRODCODE       SAVE PRODUCT                                 
*                                                                               
VKEY04   LA    R2,DELMEDH          VALIDATE MEDIA, IF ENTERED                   
         CLI   5(R2),0                                                          
         BE    VKEY06                                                           
         GOTO1 VALMED                                                           
         OI    6(R2),X'80'                                                      
         MVC   QMED,MEDIA          SAVE MEDIA                                   
*                                                                               
         LA    R2,DELMEDNH         DISPLAY MEDIA DESCRIPTION                    
         OI    6(R2),X'80'                                                      
         MVC   DELMEDN,MEDNAME                                                  
*                                                                               
VKEY06   LA    R2,DELESTH          VALIDATE ESTIMATE, IF ENTERED                
         CLI   5(R2),0                                                          
         BE    VKEY08                                                           
         MVI   ERROR,MISSING                                                    
         LA    R2,DELPRDH                                                       
         CLI   5(R2),0             MUST HAVE PRODUCT (AND CLIENT)               
         BE    ERREND              IF ESTIMATE ENTERED                          
         LA    R2,DELESTH                                                       
         GOTO1 VALEST                                                           
         OI    6(R2),X'80'                                                      
         MVC   QEST,ESTIMATE       SAVE ESTIMATE                                
*                                                                               
VKEY08   LA    R2,DELPERH          AT LEAST END PERIOD REQUIRED                 
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         GOTO1 VALPERE                                                          
         MVC   QESTART,PERIODS     SAVE START AND END OF EST PERIOD             
         MVC   QEEND,PERIODE                                                    
*                                                                               
VKEY12   LA    R2,DELOPTH          OPTION                                       
         OI    6(R2),X'80'         SET TRANSMIT                                 
         MVC   QOPT,=C'D'          ASSUME DRAFT                                 
         CLI   5(R2),0                                                          
         BE    VKEY14                                                           
         CLI   8(R2),C'D'                                                       
         BE    VKEY14                                                           
         MVI   ERROR,INVOPT                                                     
         CLI   8(R2),C'L'                                                       
         BNE   ERREND                                                           
         MVC   QOPT,8(R2)                                                       
*                                                                               
VKEY14   MVC   8(1,R2),QOPT        DISPLAY OPTION                               
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'         SET TRANSMIT                                 
*                                                                               
         MVC   CUL+1(2),RECVLEDG   RESTORE SR U/L                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VKEYX                                                            
         TM    WHEN,X'20'          TEST FOR 'SOON' REQUEST                      
         BZ    VKEYX               NO                                           
         MVI   ERROR,DRFTONLY      YES                                          
         CLI   QOPT,C'D'           MUST BE DRAFT FOR SOON                       
         BNE   ERREND                                                           
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        READ RECORDS AND FORMAT REPORT                               *         
***********************************************************************         
*                                                                               
PREP     NTR1                                                                   
         L     R1,ABOX                                                          
         MVC   AWIDE,BOXAWIDE-BOXD(R1)   ADDR OF WIDE PRINT                     
*                                                                               
         LA    R6,KEY              READ ESTIMATE RECORD                         
         USING INTRECD,R6                                                       
         XC    INTKEY,INTKEY                                                    
         MVI   INTKTYP,INTKTYPQ                                                 
         MVI   INTKSUB,INTKSUBQ                                                 
         MVC   INTKCUL,CUL                                                      
*                                                                               
         CLI   QREC,0              IS THERE A RECEIVEABLE?                      
         BE    PREP02              NO - GO READ HIGH                            
*                                                                               
         MVC   INTKACT,QREC                                                     
         CLI   QCLT,0              IS THERE A CLIENT?                           
         BE    PREP02              NO                                           
         MVC   INTKCLT,QCLT                                                     
         CLI   QPRD,0              IS THERE A PRODUCT?                          
         BE    PREP02              NO                                           
         MVC   INTKPRD,QPRD                                                     
         CLI   QMED,0              IS THERE A MEDIA?                            
         BE    PREP02              NO                                           
*                                                                               
         MVC   INTKMED,QMED                                                     
         MVC   INTKEST,QEST        MOVE EST# OR BLANKS                          
*                                                                               
PREP02   GOTO1 HIGH                                                             
         B     PREP06                                                           
*                                                                               
PREP04   GOTO1 SEQ                                                              
*                                                                               
PREP06   CLC   KEYSAVE(INTKACT-INTKEY),KEY                                      
         BNE   PREPX                                                            
         L     R6,AIO                                                           
         CLI   QREC,0              ANY RECEIVEABLE REQUESTED?                   
         BE    PREP08              NO                                           
         CLC   INTKACT,QREC        YES, DO WANT THIS ACCOUNT?                   
         BNE   PREPX               NO, WE'RE DONE                               
*                                                                               
PREP08   CLI   QCLT,0              ANY CLIENT?                                  
         BE    PREP10              NO, GO CHECK FOR PRODUCT                     
         CLC   INTKCLT,QCLT        CLIENT MATCH?                                
         BNE   PREP04              NO, READ NEXT RECORD                         
*                                                                               
PREP10   CLI   QPRD,0              ANY PRODUCT?                                 
         BE    PREP12              NO, GO CHECK FOR MEDIA                       
         CLC   INTKPRD,QPRD        PRODUCT MATCH?                               
         BNE   PREP04              NO, READ NEXT RECORD                         
*                                                                               
PREP12   CLI   QMED,0              ANY MEDIA?                                   
         BE    PREP14              NO, GO CHECK EST NUMBER                      
         CLC   INTKMED,QMED        MEDIA MATCH?                                 
         BNE   PREP04              NO, READ NEXT RECORD                         
*                                                                               
PREP14   CLI   QEST,0              ANY ESTIMATE?                                
         BE    PREP16              NO, CHECK IF OK TO DELETE                    
         CLC   INTKEST,QEST        ESTIMATE MATCH?                              
         BNE   PREP04              NO, READ NEXT RECORD                         
*                                                                               
*                                                                               
PREP16   MVI   ELCODE,IPRELQ                                                    
         BAS   RE,GETELIO          GET PROFILE ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING IPRELD,R6                                                        
         CLC   IPRSTART,QESTART    CHECK IF WITHIN START END RANGE              
         BL    PREP04                                                           
         CLC   IPREND,QEEND                                                     
         BH    PREP04                                                           
*                                                                               
         MVI   ELCODE,IESELQ                                                    
         BAS   RE,GETELIO          GET ESTIMATE ELEMENT                         
         B     *+8                                                              
*                                                                               
PREP18   BAS   RE,NEXTEL                                                        
         BNE   PREP22                                                           
*                                                                               
         USING IESELD,R6                                                        
         TM    IESSTAT,IESSPOST    REC'V POSTED?                                
         BNZ   PREP20              YES                                          
         OC    IESDAT,IESDAT       NO, IS THERE A POSTING DATE?                 
         BNZ   PREP04              YES, GET NEXT RECORD                         
*                                                                               
PREP20   CLC   IESMTH,QESTART      SEE IF WITHING DATE REQUESTED                
         BL    PREP04                                                           
         CLC   IESMTH,QEEND                                                     
         BH    PREP04                                                           
*        CP    IESREC,=P'0'        ANY RECEIVABLE AMOUNT?                       
*        BE    PREP04              NO, GET NEXT ELEMENT                         
         CP    IESREC,IESPD        YES, EQUAL AMOUNT PAID?                      
         BNE   PREP04              NO, GET NEXT ELEMENT                         
         B     PREP18              SEE IF ANYMORE                               
*                                                                               
PREP22   BAS   RE,PRINTIT          IF WE GOT THIS FAR, WE CAN DELETE            
*                                                                               
         CLI   QOPT,C'L'           ARE WE RUNNING LIVE?                         
         BNE   PREP04              NO, DONE                                     
         L     R6,AIO              YES, MARK IT FOR DELETION                    
         USING ACKEYD,R6                                                        
         OI    ACSTATUS,X'80'      TURN ON DELETE BIT                           
         GOTO1 WRITE                                                            
         B     PREP04              GET NEXT RECORD                              
*                                                                               
PREPX    B     XIT                 DONE                                         
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT AND PRINT REPORT                                      *         
***********************************************************************         
*                                                                               
PRINTIT  NTR1                                                                   
         USING WIDED,R3                                                         
         USING PRTD,R4                                                          
         USING INTRECD,R6                                                       
         L     R3,AWIDE                                                         
         LA    R4,P                                                             
         L     R6,AIO                                                           
         MVC   P,SPACES                                                         
         MVC   PRTREC,INTKACT                                                   
         MVC   PRTCLT,INTKCLT                                                   
         MVC   PRTPRD,INTKPRD                                                   
         MVC   PRTEST,INTKEST                                                   
         MVC   PRTMED(L'INTKMED),INTKMED                                        
         TM    (ACSTATUS-ACKEYD)(R6),X'02'   IS MEDIA AN MI RECORD?             
         BZ    PRINT2                                                           
         MVC   PRTMED(3),=C'MI='                                                
         MVC   PRTMED+3(L'INTKMED),INTKMED                                      
*                                                                               
PRINT2   MVI   ELCODE,IPRELQ       GET PROFILE ELEMENT AGAIN                    
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE BY NOW                         
*                                                                               
         USING IPRELD,R6                                                        
         MVC   WORK(2),IPRSTART                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(18,PRTSTR)                                 
         MVC   WORK(2),IPREND                                                   
         GOTO1 DATCON,DMCB,(1,WORK),(18,PRTEND)                                 
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
*        SPECS FOR REPORT                                             *         
***********************************************************************         
*                                                                               
MYSPECS  DS    0D                                                               
         SSPEC H1,2,AGYNAME                                                     
         SSPEC H2,2,CREATED                                                     
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        HEAD HOOK                                                    *         
***********************************************************************         
*                                                                               
HOOK     NTR1                                                                   
         LA    R4,H1+48                                                         
         MVC   0(24,R4),=C'INTERAGENCY PROFILE PEEL'                            
         GOTO1 CENTER,DMCB,(R4),24                                              
         GOTO1 UNDERLIN,DMCB,(24,(R4)),(X'BF',132(R4))                          
*                                                                               
         MVC   H8+1(8),=C'RCV ACCT'                                             
*                                                                               
         MVC   H8+14(3),=C'CLT'                                                 
         MVC   H8+18(3),=C'PRD'                                                 
*                                                                               
         MVC   H8+22(8),=C'ESTIMATE'                                            
         MVC   H8+31(5),=C'MEDIA'                                               
         MVC   H7+38(12),=C'---PERIOD---'                                       
         MVC   H8+37(5),=C'START'                                               
         MVC   H8+43(3),=C'END'                                                 
*                                                                               
         CLI   QOPT,C'D'           IS THIS A DRAFT?                             
         BNE   HOOK1               NO                                           
         LA    R4,H3+48                                                         
         MVC   0(24,R4),=C'******* D R A F T ******'                            
         GOTO1 CENTER,DMCB,(R4),24                                              
*                                                                               
HOOK1    L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         LTR   R4,R4                                                            
         BZ    HOOKX                                                            
         L     R2,AWIDE                                                         
         USING WIDED,R2                                                         
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVC   BOXWIDTH,=A(L'P)                                                 
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,TOP                                                    
         MVI   BOXROWS+8,MIDLINE                                                
         MVI   BOXROWS+57,BOTTOM                                                
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         LA    R1,BOXCOLS                                                       
         MVI   PRTBOXL-PRTD(R1),LEFTMAR                                         
         MVI   PRTBOX1-PRTD(R1),COLUMN                                          
         MVI   PRTBOX2-PRTD(R1),COLUMN                                          
         MVI   PRTBOX3-PRTD(R1),COLUMN                                          
         MVI   PRTBOX4-PRTD(R1),COLUMN                                          
         MVI   PRTBOX5-PRTD(R1),COLUMN                                          
*        MVI   PRTBOX6-PRTD(R1),COLUMN                                          
         MVI   PRTBOXR-PRTD(R1),RIGHTMAR                                        
HOOKX    B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
TOP      EQU   C'T'                TOP OF BOX                                   
MIDLINE  EQU   C'M'                MIDLINE OF BOX                               
BOTTOM   EQU   C'B'                BOTTOM OF BOX                                
LEFTMAR  EQU   C'L'                LEFT MARGIN OF BOX                           
RIGHTMAR EQU   C'R'                RIGHT MARGIN OF BOX                          
COLUMN   EQU   C'C'                COLUMN OF BOX                                
FOXES    DC    X'FFFF'             X'FF'                                        
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
* DSECT TO COVER PRINT LINE                                     *               
*****************************************************************               
*                                                                               
PRTD     DSECT                                                                  
PRTBOXL  DS    C                                                                
PRTREC   DS    CL12                RECEIVEABLE                                  
PRTBOX1  DS    C                                                                
PRTCLT   DS    CL3                 CLIENT                                       
PRTBOX2  DS    C                                                                
PRTPRD   DS    CL3                 PRODUCT                                      
PRTBOX3  DS    C                                                                
PRTEST   DS    CL6                 ESTIMATE NUMBER                              
         DS    CL2                                                              
PRTBOX4  DS    C                                                                
PRTMED   DS    CL5                 MEDIA                                        
PRTBOX5  DS    C                                                                
PRTSTR   DS    CL6                 PERIOD START                                 
         DS    C                                                                
PRTEND   DS    CL6                 PERIOD END                                   
PRTBOXR  DS    C                                                                
PRTDLNQ  EQU   *-PRTD                                                           
         EJECT                                                                  
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACINTWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACINTWORKD                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDWIDED                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
T619FFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACINTF6D                                                       
         EJECT                                                                  
*****************************************************************               
* DSECT TO COVER LOCAL WORKING STORAGE                          *               
*****************************************************************               
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   LOCAL                                                            
QREC     DS    CL12                                                             
QCLT     DS    CL3                                                              
QPRD     DS    CL3                                                              
QMED     DS    CL2                                                              
QEST     DS    CL6                                                              
QESTART  DS    CL2                                                              
QEEND    DS    CL2                                                              
QOPT     DS    CL1                                                              
QLNQ     EQU   *-QREC                                                           
*                                                                               
AWIDE    DS    A                                                                
LOCALLN  EQU   *-LOCAL                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACINT16S  05/01/02'                                      
         END                                                                    

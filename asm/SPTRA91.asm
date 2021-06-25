*          DATA SET SPTRA91    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T21691A                                                                  
*INCLUDE KHDUMMY                                                                
*                                                                               
***********************************************************************         
*  NUMBER:                                                            *         
*  TITLE: T21691 - NETWORK TRAFFIC BILLING PROGRAM FOR MARGA          *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM PRODUCES A BILLING REPORT FOR NETWORK       *         
*            TRAFFIC                                                  *         
*                                                                     *         
*  CALLED FROM: TRAFFIC CONTROLLER (T21600), WHICH CALLS              *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATA MANAGER                                          *         
*               NETIO                                                 *         
*                                                                     *         
*  INPUTS: SEE SCREEN SPTRAAC (T216AC) ONLY USES PERIOD FIELD.        *         
*          SPTRAWORKD (SYSD)                                          *         
*          DDSPLWORKD (GEND)                                          *         
*                                                                     *         
*  OUTPUTS: REPORT WITH BILLING AMOUNTS BY CLIENT                     *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - POINTER TO NETBLOCKD                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE REGISTER                                  *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - CLIENT RECS IN FCLT RTN                          *         
*             AIO2 -                                                  *         
*             AIO3 - UNIT RECORDS                                     *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************                                               *         
*  LOGIC: READ UNIT RECORDS, ANY MONTH THAT HAS X'21' COMMERCIAL      *         
*  ELEMENTS FOR A CLIENT IS CONSIDERED BILLABLE AT 25% OF THE         *         
*  NETWORK RATE.                                                      *         
*                                                                     *         
*  LEV  3    OCT13/89 FIX CLUNPK ERROR                                *         
*  LEV  4    MAR27/90 FIX USING EMPTY 21 ELEMS AS BILLABLE            *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21691 NETWORK TRAFFIC BILLING REPORT'                          
T21691   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BILL**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,STNETBLK                                                      
         USING NETBLOCKD,R5                                                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFFLINE LIST RECORDS                         
         BE    LRR                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE ONLY VALIDATES PERIOD FOR BILLING *                      
         SPACE                                                                  
VK       LA    R2,TRAPERH                                                       
         SPACE                                                                  
* VALIDATE PERIOD - MONTH AND YR, OR IF NOT ENTERED                             
*                   CALC LAST MONTH                                             
         SPACE                                                                  
         CLI   5(R2),0             IF NO DATE ENTERED, USE SYSTEM DATE          
         BE    VK20                                                             
         SPACE                                                                  
         GOTO1 DATVAL,DMCB,(2,8(R2)),USERQSTR                                   
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         SPACE                                                                  
* CALC CALENDAR START/END OF MONTH DATES *                                      
         SPACE                                                                  
         MVC   USERQSTR+4(2),=C'01'                                             
         GOTO1 ADDAY,(R1),USERQSTR,USERQEND,F'31'                               
VK10     GOTO1 (RF),(R1),USERQEND,USERQEND,F'-1'                                
         CLC   USERQSTR(4),USERQEND                                             
         BNE   VK10                                                             
         B     VK30                                                             
         SPACE                                                                  
* CALCULATE LAST MONTH FOR BILLING                                              
         SPACE                                                                  
VK20     GOTO1 DATCON,DMCB,(5,0),(0,USERQEND)                                   
         SPACE                                                                  
         MVC   USERQEND+4(2),=C'01'                                             
         GOTO1 ADDAY,(R1),USERQEND,USERQEND,F'-1'                               
         MVC   USERQSTR,USERQEND                                                
VK24     GOTO1 (RF),(R1),USERQSTR,USERQSTR,F'-1'                                
         CLC   USERQEND(4),USERQSTR                                             
         BE    VK24                                                             
         GOTO1 (RF),(R1),USERQSTR,USERQSTR,F'1'                                 
*                                                                               
VK30     BAS   RE,SETUNIT          READ FROM UNIT FILE                          
         XC    CLTOTAL(60),CLTOTAL                                              
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* OFFLINE LIST ROUTINE *                                                        
*                                                                               
LRR      LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         SPACE                                                                  
         BAS   RE,NETI             INIT NETBLOCK                                
         SPACE                                                                  
LRR10    GOTO1 ANETIO,DMCB,(R5)                                                 
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    LRR70                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRR10                                                            
*                                                                               
         MVC   SVBCLT,NBACTCLI                                                  
         B     LRR30                                                            
*                                                                               
* ADD TO COUNTS                                                                 
*                                                                               
LRR20    TM    NBUNITST,X'42'      IF PRE-EMPT OR MISSED                        
         BZ    LRR22                                                            
         XC    NBACTUAL,NBACTUAL     SET COSTS TO 0                             
         XC    NBASSIGN,NBASSIGN     BUT LEAVE BILLING AND PAYING               
         XC    NBINTEG,NBINTEG                                                  
*                                                                               
LRR22    ICM   R0,15,NBACTUAL                                                   
         A     R0,ORDACT                                                        
         ST    R0,ORDACT                                                        
         L     R1,NBAIO                                                         
         USING NURECD,R1                                                        
         LA    R6,NUMAINEL         SET UP R6 FOR GETEL                          
         DROP  R1                                                               
         MVI   ELCODE,0                                                         
LRR24    BAS   RE,NEXTEL                                                        
         BNE   LRR30               LAST ELEMENT                                 
         CLI   0(R6),X'10'         BILLING ELEMENT                              
         BE    DOBILL                                                           
         CLI   0(R6),X'12'         PAYING ELEMENT                               
         BE    DOPAY                                                            
         B     LRR24                                                            
*                                                                               
         USING NUBILD,R6                                                        
DOBILL   TM    NUBILST,X'20'                                                    
         BO    LRR24                                                            
         ICM   RE,15,NUBILGRS         GET GRS FIGURE                            
         A     RE,BILLG                                                         
         ICM   RF,15,NUBILNET         GET NET FIGURE                            
         A     RF,BILLN                                                         
         STM   RE,RF,BILLG                                                      
         B     LRR24                                                            
         DROP  R6                                                               
*                                                                               
         USING NUPAYD,R6                                                        
DOPAY    ICM   RE,15,NUPAYGRS      GET GROSS PAID                               
         A     RE,CLEARG                                                        
         ICM   RF,15,NUPAYNET      GET NET FIGURE                               
         A     RF,CLEARN                                                        
         STM   RE,RF,CLEARG                                                     
         B     LRR24                                                            
         DROP  R6                                                               
         SPACE                                                                  
* CHECK FOR ANY TRAFFIC ELEMENTS *                                              
         SPACE                                                                  
LRR30    L     R6,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR34                                                            
         OC    2(40,R6),2(R6)      CK IF EMPTY ELEM                             
         BNZ   LRR36                                                            
         OC    43(9,R6),43(R6)     CK IF EMPTY ELEM                             
         BNZ   LRR36                                                            
         SPACE                                                                  
LRR34    L     R6,NBAIO                                                         
         MVI   ELCODE,X'23'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR40                                                            
         SPACE                                                                  
* SET TRAFFIC FLAG *                                                            
         SPACE                                                                  
LRR36    MVI   TRAFLAG,C'Y'                                                     
         SPACE                                                                  
LRR40    GOTO1 ANETIO,DMCB,(R5)                                                 
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    LRR50                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRR40                                                            
         CLC   SVBCLT,NBACTCLI                                                  
         BE    LRR30                                                            
         SPACE                                                                  
LRR50    CLI   TRAFLAG,C'Y'        THIS A TRAFFIC CLIENT                        
         BNE   LRR60                                                            
         SPACE                                                                  
         BAS   RE,SETSPOT          SET FROM NET TO SPOT                         
         BAS   RE,FCLT                                                          
         BAS   RE,SETUNIT          SET FROM SPOT TO NET                         
         SPACE                                                                  
         MVC   PCLT,QCLT                                                        
         MVC   PCLTNM,CLTNM                                                     
         SPACE                                                                  
*        EDIT  (B4,ORDACT),(12,PORDACT),2                                       
*        EDIT  (B4,CLEARG),(12,PCLRG),2                                         
*        EDIT  (B4,CLEARN),(12,PCLRN),2                                         
*        EDIT  (B4,BILLG),(12,PBILLG),2                                         
*        EDIT  (B4,BILLN),(12,PBILLN),2                                         
         SPACE                                                                  
         LM    R0,R4,ORDACT                                                     
         A     R0,TORDACT                                                       
         A     R1,TCLEARG                                                       
         A     R2,TCLEARN                                                       
         A     R3,TBILLG                                                        
         A     R4,TBILLN                                                        
         STM   R0,R4,TORDACT                                                    
         SPACE                                                                  
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* NOW ADD TO AGENCY TOTALS *                                                    
*                                                                               
LRR60    LM    R0,R4,ORDACT                                                     
         A     R0,AORDACT                                                       
         A     R1,ACLEARG                                                       
         A     R2,ACLEARN                                                       
         A     R3,ABILLG                                                        
         A     R4,ABILLN                                                        
         STM   R0,R4,AORDACT                                                    
         XC    CLTOTAL,CLTOTAL     CLEAR OUT CLIENT TOTALS                      
         MVI   TRAFLAG,0           RESET TRAFFIC FOUND FLAG                     
         MVC   SVBCLT,NBACTCLI                                                  
         SPACE                                                                  
         CLI   NBMODE,NBREQLST     END OF AGENCY                                
         BNE   LRR30                                                            
         SPACE                                                                  
         B     EXIT                                                             
LRR70    MVC   PCLT(14),=C'TRAFFIC TOTALS'                                      
         SPACE                                                                  
         EDIT  (B4,TORDACT),(12,PORDACT),2                                      
         EDIT  (B4,TCLEARG),(12,PCLRG),2                                        
         EDIT  (B4,TCLEARN),(12,PCLRN),2                                        
         EDIT  (B4,TBILLG),(12,PBILLG),2                                        
         EDIT  (B4,TBILLN),(12,PBILLN),2                                        
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVC   PCLT(13),=C'AGENCY TOTALS'                                       
         SPACE                                                                  
         EDIT  (B4,AORDACT),(12,PORDACT),2                                      
         EDIT  (B4,ACLEARG),(12,PCLRG),2                                        
         EDIT  (B4,ACLEARN),(12,PCLRN),2                                        
         EDIT  (B4,ABILLG),(12,PBILLG),2                                        
         EDIT  (B4,ABILLN),(12,PBILLN),2                                        
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
         SPACE                                                                  
FCLT     NTR1                                                                   
         MVC   AIO,AIO1                                                         
         LA    R1,KEY                                                           
         USING CLTHDR,R1                                                        
         XC    KEY,KEY                                                          
         MVC   CKEYAM,NBACTAM                                                   
         MVC   CKEYCLT,SVBCLT                                                   
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING CLTHDR,R1                                                        
         MVC   CLTNM,CNAME                                                      
         IC    R0,CPROF+6                                                       
         DROP  R1                                                               
         GOTO1 CLUNPK,DMCB,((R0),SVBCLT),QCLT                                   
         MVC   SVBCLT,NBACTCLI                                                  
         B     EXIT                                                             
         SPACE 2                                                                
* INITIALIZE NETBLOCK *                                                         
         SPACE                                                                  
NETI     NTR1                                                                   
         XC    0(256,R5),0(R5)     CLEAR NETBLOCK                               
         XC    256(256,R5),256(R5)                                              
         XC    512(256,R5),512(R5)                                              
         XC    768(256,R5),768(R5)                                              
         SPACE                                                                  
         MVC   NBSELAGY,AGENCY                                                  
         MVC   NBEFFAGY,AGENCY                                                  
         MVC   NBSELCLI,=C'ALL'                                                 
         MVC   NBSELPRD,=C'ALL'                                                 
         MVI   NBSELPST,0                                                       
         MVC   NBSELSTR(12),USERQSTR                                            
         MVI   NBSELUOP,C'A'                                                    
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBMODE,NBPROCUN                                                  
         MVC   NBAIO,AIO3                                                       
         L     R1,SYSPARMS                                                      
         L     RF,16(,R1)           COMFACS ADDRESS                             
         ST    RF,NBACOM                                                        
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A27')                                 
         L     RF,0(,R1)                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ANETIO                                                        
         B     EXIT                                                             
         EJECT                                                                  
* SET SPOT FILE PARAMETERS                                                      
*                                                                               
SETSPOT  MVC   SYSDIR(2),=C'SP'                                                 
         MVC   SYSFIL(2),=C'SP'                                                 
         MVC   LKEY,=H'13'                                                      
         MVC   DATADISP,=H'24'                                                  
         BR    RE                                                               
         SPACE 2                                                                
* SET UNIT FILE PARAMETERS                                                      
*                                                                               
SETUNIT  MVC   SYSDIR(2),=C'UN'                                                 
         MVC   SYSFIL(2),=C'UN'                                                 
         MVC   LKEY,=H'20'                                                      
         MVC   DATADISP,=H'27'                                                  
         BR    RE                                                               
         SPACE 2                                                                
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         GOTO1 ERREX                                                            
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,30,C'TRAFFIC BILLING REPORT'                                  
         SSPEC H2,30,C'----------------------'                                  
         SSPEC H1,65,AGYNAME                                                    
         SSPEC H2,65,AGYADD                                                     
         SSPEC H3,30,PERIOD                                                     
         SSPEC H3,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H4,78,RUN                                                        
         SSPEC H4,65,REPORT                                                     
         SSPEC H5,65,REQUESTOR                                                  
         SSPEC H5,95,PAGE                                                       
         SSPEC H6,3,C'CLIENT'                                                   
         SSPEC H7,3,C'------'                                                   
         SSPEC H6,30,C'PERCENT'                                                 
         SSPEC H7,30,C'CLEARED'                                                 
         SSPEC H6,41,C'ORD ACTUAL'                                              
         SSPEC H7,41,C'----------'                                              
         SSPEC H6,54,C'CLR GROSS'                                               
         SSPEC H7,54,C'----------'                                              
         SSPEC H6,67,C'CLR NET'                                                 
         SSPEC H7,67,C'-------'                                                 
         SSPEC H6,80,C'BILL GROSS'                                              
         SSPEC H7,80,C'----------'                                              
         SSPEC H6,93,C'BILL NET'                                                
         SSPEC H7,93,C'--------'                                                
         DC    X'00'               END SSPECS                                   
         SPACE                                                                  
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAACD                                                       
NETBLOCKD DSECT                                                                 
       ++INCLUDE NETBLOCKD                                                      
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
* START OF MISC STORAGE *                                                       
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPARE                                                          
ANETIO   DS    A                                                                
*                                                                               
* CLIENT TOTALS                                                                 
*                                                                               
CLTOTAL  DS   0XL20                                                             
ORDACT   DS    F                                                                
CLEARG   DS    F                                                                
CLEARN   DS    F                                                                
BILLG    DS    F                                                                
BILLN    DS    F                                                                
*                                                                               
* AGENCY TRAFFIC TOTALS                                                         
*                                                                               
TORDACT  DS    F                                                                
TCLEARG  DS    F                                                                
TCLEARN  DS    F                                                                
TBILLG   DS    F                                                                
TBILLN   DS    F                                                                
*                                                                               
* AGENCY TOTALS                                                                 
*                                                                               
AORDACT  DS    F                                                                
ACLEARG  DS    F                                                                
ACLEARN  DS    F                                                                
ABILLG   DS    F                                                                
ABILLN   DS    F                                                                
*                                                                               
FLDH     DS    XL8                                                              
FLD      DS    CL64                                                             
*                                                                               
TRAFLAG  DS    CL1                 SET TO Y IF ANY TRAFFIC ELEMENTS             
*                                                                               
SVBCLT   DS    XL2                                                              
STNETBLK DS    0D                                                               
         DS    CL1024                                                           
ENDSYSD  EQU   *                                                                
         EJECT                                                                  
* OFFLINE REPORT LINE                                                           
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PCLTNM   DS    CL20                                                             
         DS    CL2                                                              
PPERCLR  DS    CL7                                                              
         DS    CL3                                                              
PORDACT  DS    CL12                                                             
         DS    CL1                                                              
PCLRG    DS    CL12                                                             
         DS    CL1                                                              
PCLRN    DS    CL13                                                             
         DS    CL1                                                              
PBILLG   DS    CL12                                                             
         DS    CL1                                                              
PBILLN   DS    CL12                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPTRA91   05/01/02'                                      
         END                                                                    

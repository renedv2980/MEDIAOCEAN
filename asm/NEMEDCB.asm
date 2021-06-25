*          DATA SET NEMEDCB    AT LEVEL 055 AS OF 08/19/11                      
*PHASE T31ECBA,*                                                                
*INCLUDE CLUNPK                                                                 
*INCLUDE NETNET                                                                 
*INCLUDE COVAIL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T31ECB-NETWORK CLOSEOUT REPORT'                                 
** TEST VERSION FOR I8RQFIL                                                     
T31ECB   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NECB**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
***      L     R1,ANETWS3             ANETWS3=CLIST                             
***      ST    R1,ACLISTSV                                                      
         L     R1,ANETWS4             ANETWS4+2000=CLIST                        
         LA    R1,2000(R1)                                                      
         ST    R1,ACLISTSV                                                      
         LA    R6,1(RB)                                                         
         LA    R6,4095(R6)                                                      
         USING T31ECB,RB,R6        R6-SECOND BASE REGISTER                      
         L     R1,=A(HEADING)                                                   
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         L     R1,=A(BREAKTOT)                                                  
         ST    R1,ABRKTOT                                                       
         L     R1,=A(ERRORS)                                                    
         ST    R1,AERRORS                                                       
         L     R1,=A(GOALDNR)                                                   
         ST    R1,AGOALDNR                                                      
         L     R1,=A(GOALDNR)                                                   
         ST    R1,AGOALDNR                                                      
         L     R1,=A(GETCLST)                                                   
         ST    R1,AGETCLST                                                      
         L     R1,=A(GETEST)                                                    
         ST    R1,AGETEST                                                       
         CLI   DELETESW,C'Y'                                                    
         BNE   *+16                                                             
         CLI   TWAWRITE,C'Y'                                                    
         BE    *+8                                                              
         MVI   DELETESW,C'N'                                                    
         EJECT                                                                  
*HIPO******************************************************************         
*  TITLE: NEMEDPZ (T31EPZ) NETWORK INTERFACE TAPE                    *          
*                                                                     *         
*  COMMENTS: NETWORK CLOSEOUT REPORT                                 *          
*                                                                     *         
*  CALLS TO: NETIO                                                              
*                                                                     *         
*  GLOBAL: R7-MYWORKD (ANETWS2+500)                                   *         
*                                                                     *         
***********************                                               *         
*  LOGIC:                                                                       
*         MAINLINE = PROCUN: READS UNITS, CHKS FOR ERRORS                       
*                            IN UNIT BILL/PAID, SAVES DISK ADR                  
*                                                                               
*                    PROCBILL: READS BILLING RECS/CHKS THAT                     
*                              BILLING REC TOT=UNIT BILLED TOT                  
*                                                                               
*                    PROCDNR: DELETE N' RITE, THIS ROUTINE DOES                 
*                             NOT DO ANY FURTHER ERROR CHKING.                  
*                             REREADS UNITS THRU NETIO BY PASSING               
*                             SAVED DISK ADRS, MARKS THEM CLOSED                
*                             AND WRITES REPORT.                                
*                             REREADS BILL RECS,MARKS THEM CLOSED               
*                             AND WRITES REPORT.                                
*                                                                     *         
*ENDHIPO***************************************************************         
         SPACE 3                                                                
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
         CLI   MODE,RUNFRST        *RUNFIRST                                    
         BNE   NXTMD                                                            
         GOTO1 =A(FIRSTRUN),DMCB,(RC)                                           
*                                                                               
NXTMD    CLI   MODE,RUNLAST          * RUNLAST                                  
         BNE   EXIT                                                             
         GOTO1 =A(LASTRUN),DMCB,(RC)                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*****************************************                                       
* PROCESS UNITS AND BILLS                                                       
*                                                                               
*****************************************                                       
LR       DS    0H                                                               
         BRAS  RE,LRINIT                                                        
*                                                                               
*                                                                               
         BAS   RE,CLEARTOT                                                      
         BAS   RE,CLRUNTOT                                                      
         MVI   ESTHDOPT,0                                                       
         MVI   NBDATA,C'U'               SET UP FOR UNIT RECS                   
         MVI   NBSEQ,C'P'          EXPECTS X'94' KEY SEE DNT10                  
         MVI   NBSELPST,C'B'             LOCKED AND UNLOCKED                    
         MVI   NBUSER+13,C'N'            OVERRIDE PROF. GET PRE-EMPTS           
         MVI   FRST,1                                                           
         OI    NBINDS2,NBBILLRD+NBLCMPR  NEW BILLING RECS                       
*                                                                               
*                              CHK PU PROFILE OF 1ST CLIENT                     
*                              VALIDATED IN EDIT MODULE                         
         CLI   PUFSW,C'N'          CHECK PU PROFILE?                            
         BE    LR10                NO                                           
         CLI   PUFSW,C'Y'          CHECK PU PROFILE?                            
         BE    LR05                YES                                          
*                                                                               
         CLI   PUFSW,C'O'          ONLY PROCESS PU PROFILE CLIENTS              
         BE    *+6                                                              
         DC    H'0'                CAN NOT BE                                   
         GOTO1 =A(CHKPU),DMCB,(RC)             USE THIS CLIENT?                 
         BE    LR10                            YES                              
         B     LR07                            NO                               
*                                                                               
LR05     GOTO1 =A(CHKPU),DMCB,(RC)             SKIP THIS CLIENT?                
         BNE   LR10                            NO                               
LR07     CLC   =C'ALL',NBSELCLI               .. YES/IF CLI NOT=ALL             
         BNE   EXIT                           .. EXIT                           
         MVI   NBFUNCT,NBFNXCLT               .. ELSE/FIND NEXT CLT             
*                                                                               
LR10     DS    0H                                                               
         NETGO NSNETIO,DMCB,NETBLOCK     GET UNIT RECS                          
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   FRST,1                                                           
         BNE   LR11                                                             
         CLC   NBSELCLI(3),=C'ALL'                                              
         BNE   LR11                                                             
         GOTO1 AGETCLST,DMCB,(RC)                                               
*                                                                               
         BAS   RE,GETPRF                                                        
*                                                                               
LR11     CLI   NBMODE,NBVALCLI      TEST NEW CLIENT                             
         BNE   LR14                                                             
         L     R2,NBAIO             SAVE NEW CLIST FOR NEW CLIENT               
         USING CLTHDR,R2                                                        
***      L     RF,ANETWS4                                                       
***      LA    RF,2000(RF)                                                      
         L     RF,ACLISTSV                                                      
         MOVE  ((RF),880),CLIST                                                 
         MVC   NXTOFFCD,COFFICE                                                 
         DROP  R2                                                               
         OC    ESTKEYSV,ESTKEYSV   IS THERE DATA/IF NOT SKIP PREVIOUS           
         BZ    LR11B   PXZCHANGE   (E.G. CLIENT HEADER BUT NO UNITS)            
                                                                                
         BAS   RE,PROCBILL                                                      
                                                                                
         CLI   ERRORSW,0           IS PREV EST = ERROR                          
         BE    LR12                                                             
LR11B    MVI   ERRORSW,0           YES/RESET ERR SWITCH                         
         B     LR12A                                                            
LR12     BAS   RE,PROCDNR          NO/CLOSE OUT                                 
         GOTO1 =A(SUMMARY),DMCB,(RC)                                            
*                                     SET UP FOR NEW CLIENT                     
LR12A    XC    ESTKEYSV(14),ESTKEYSV     CLEAR EST SAVE                         
         MVC   OFFCDE,NXTOFFCD           SET NEW OFFICE CODE                    
*******************************************************************             
         CLI   PUFSW,C'O'          ONLY PROCESS PU PROFILE CLIENTS              
         BNE   LR12C                                                            
         GOTO1 =A(CHKPU),DMCB,(RC)             USE THIS CLIENT?                 
         BE    LR13                            YES                              
         B     LR12D                           NO                               
*                                                                               
*                                  CHK PU PROFILE SAYS SKIP CLIENT?             
LR12C    CLI   PUFSW,C'N'          READ PU PROFILE?                             
         BE    LR13                                                             
         GOTO1 =A(CHKPU),DMCB,(RC)                                              
         BNE   *+12                                                             
LR12D    MVI   NBFUNCT,NBFNXCLT    FIND NEXT CLT                                
         B     LR10                                                             
LR13     DS    0H                                                               
********************************************************************            
         BAS   RE,GETPRF                 GET NEW PROFLIE                        
         MVI   NEWCLI,C'Y'               SET NEW CLIENT SWITCH                  
         MVI   ESTHDOPT,0                                                       
         MVI   NBUSER+13,C'N'            OVERRIDE PROF. GET PRE-EMPTS           
*                                        MUST BE SET FOR EACH CLIENT            
         B     LR10                      GET NEXT REC                           
         EJECT                                                                  
LR14     CLI   NBMODE,NBPROCUN         PROCESS UNIT                             
         BNE   LR16                                                             
         CLI   FRST,1                                                           
         BNE   LR14A                                                            
         BAS   RE,GETPRF                                                        
LR14A    BAS   RE,PROCUN                                                        
         CLI   ESTHDERR,C'Y'       WAS THERE EST ERR                            
         BNE   LR10                  NO/GET NEXT RECORD                         
         MVI   ESTHDERR,0            YES/CLEAR ERR SWITCH                       
         XC    ESTKEYSV(14),ESTKEYSV     CLEAR ESTKEYSV                         
         B     LR11                                                             
*                                                                               
LR16     CLI   NBMODE,NBREQLST     LAST RECORD                                  
         BNE   LR10                                                             
         CLI   ERRORSW,0           IF ERROR ON/ GO TO REPORT SUMMARY            
         BNE   LR18                                                             
         OC    ESTKEYSV(14),ESTKEYSV    WAS THERE ANY DATA                      
         BZ    LR18                  PXZ FUDGE                                  
         BAS   RE,PROCBILL                                                      
         CLI   ERRORSW,0                                                        
         BNE   *+8                                                              
         BAS   RE,PROCDNR                                                       
LR18     GOTO1 =A(SUMMARY),DMCB,(RC)                                            
         GOTO1 =A(RUNFINAL),DMCB,(RC)                                           
*                                                                               
         CLI   PUFSW,C'O'           ONLY PROCESS PU PROFILE CLIENTS             
         BE    *+8                 YES-DON'T DO TRAFFIC                         
         BRAS  RE,ADDREQS          ADD I8/TR REQUEST FILES                      
*                                                                               
LRXX     B     EXIT                                                             
         EJECT                                                                  
******************************************                                      
* PROCESS UNITS                                                                 
* UNIT  BILLED-PAYED DOLS MUST EQUAL                                            
* UNIT BILL MONTH CANNOT EQUAL CURRENT MONTH                                    
* ADD TO UNTBILLED/UNTPAID                                                      
* SAVE DISK ADDRESSES OF UNITS FOR USE IN DELETE/PRINT                          
*                                                                               
PROCUN   NTR1                                                                   
         BAS   RE,GETSPC                                                        
         CLC   ESTBSV,NBACTEST      TEST IF NOT SAME ESTIMATE                   
         BE    PUN3                                                             
         GOTO1 AGETEST,DMCB,(RC)                                                
         CLI   ESTHDERR,C'Y'       WAS THERE AN ESTIMATE ERR                    
         BE    PUNX                                                             
         CLI   NBFUNCT,NBFNXEST       IS EST OUT OF RANGE                       
         BE    PUNX                                                             
         CLI   FRST,0              .IS IT FIRST TIME                            
         BE    PUN0                                                             
         MVI   FRST,0                .YES/CLEAR FRST                            
         L     R1,ADISKSV            .CLEAR 1ST 5 BYTES DSKSV AREA              
         XC    0(5,R1),0(R1)                                                    
         MVC   ESTKEYSV(14),NEWESTK       SET ESTIMATE                          
         MVI   NEWCLI,0          IN CASE 1ST CLI(IN EDIT) HAD NO DATA           
         B     PUN3                       AND PROCESS UNIT                      
PUN0     DS    0H                    .NO/                                       
         CLI   NEWCLI,C'Y'               IF NEW CLIENT                          
         BE    PUN0A                                                            
         CLI   ERRORSW,0                 IF  FROM ERROR                         
         BE    PUN1                         (IF NOT DO OLD EST)                 
PUN0A    MVI   ERRORSW,0                       CLEAR ERROR                      
         MVI   NEWCLI,0                        CLEAR NEW CLI                    
         MVC   ESTKEYSV(14),NEWESTK             SET NEW ESTIMATE                
         B     PUN3                             AND PROCESS THE UNIT            
*                                                                               
PUN1     BAS   RE,PROCBILL             PROCESS OLD ESTIMATE                     
         CLI   ERRORSW,0                                                        
         BNE   *+8                 IF ERROR/SKIP DELETE/PROCESS UNIT            
         BAS   RE,PROCDNR                                                       
         MVI   ERRORSW,0           CLEAR ERROR SW                               
         MVI   UNTWARN,0           CLEAR ERROR SW                               
         MVC   ESTKEYSV(14),NEWESTK    SET NEW ESTIMATE                         
*                                                                               
PUN3     DS    0H                                                               
*        GOTO1 HEXOUT,DMCB,NBAIO,P+10,40                                        
*        MVC   P(4),=C'UNIT'                                                    
*        GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,UNTBLPD      UNIT BILL-PAY SWITCHES/DOLS MATCH                
         CLI   ERRORSW,0                                                        
         BNE   PUNXX                                                            
         BAS   RE,CURMNTH          DOES BILL MONTH = CURRENT MONTH              
         CLI   ERRORSW,0                                                        
         BNE   PUNXX                                                            
         BAS   RE,UNTOBP       UNIT ORDERED=BILLED=PAID                         
         CLI   ERRORSW,0                                                        
         BNE   PUNXX                                                            
         BAS   RE,ADDTOTS          ADD UNIT BILLED/PAID TO TOTALS               
         LA    R2,NBKEY                                                         
         USING NURECD,R2                                                        
         L     R3,ADISKSV                                                       
***      GOTO1 =V(PRNTBL),DMCB,=C'AA',0(R3),C'DUMP',30,=C'1D'                   
PUN5     OC    0(4,R3),0(R3)     SAVE DISK ADDRESS OF UNITS                     
         BZ    *+12                                                             
         LA    R3,5(R3)                                                         
         B     PUN5                                                             
         MVC   0(4,R3),NUDA                                                     
         CLI   UNTWARN,0                                                        
         BE    PUN7                                                             
         MVI   4(R3),C'*'          SET WARNING FOR UNIT                         
         CLI   UNTWARN,1                                                        
         BNE   *+8                                                              
         MVI   4(R3),C'B'          SET IN BILLING MONTH ERR                     
         MVI   UNTWARN,0                                                        
PUN7     XC    5(5,R3),5(R3)       CLEAR FOLLOWING 5 BYTES                      
         L     R1,UNTCNTR                                                       
         LA    R1,1(R1)                                                         
         ST    R1,UNTCNTR                                                       
         C     R1,TBLMAXRC        MAX NUM OF UNITS                              
         BNH   PUNX                                                             
         DC    H'0'                OVER MAX NUM OF UNITS                        
PUNX     DS    0H                                                               
         B     EXIT                                                             
         DROP  R2                                                               
PUNXX    BAS   RE,CLEARTOT     COMES HERE IF SKIPPING ERRS/CLEAR TOTS           
         L     R1,ADISKSV                        BEFORE NEXT EST                
         XC    0(5,R1),0(R1)       CLEAR START OF UNT DSKADDR AREA              
         XC    ESTKEYSV,ESTKEYSV   PXZCHANGE                                    
         B     EXIT                                                             
         EJECT                                                                  
*********************************************                                   
*    PROCESS BILLS                                                              
*    IF BILLS TOTS EQUAL UNIT TOTS                                              
*    THEN ALL IS COPACETIC SO CLOSEOUT                                          
*                                                                               
PROCBILL NTR1                                                                   
         BAS   RE,GETBILL          PROCESS BILL RECORDS                         
         BAS   RE,GETMAN           PROCESS MANUAL BILLS                         
         BAS   RE,UNTNBLS          TEST BILL=UNT TOTS                           
         B     EXIT                                                             
         SPACE 2                                                                
**************************************                                          
* DELETE AND PRINT ROUTINES                                                     
*                                                                               
PROCDNR  NTR1                                                                   
         BAS   RE,ESTDNR           DELETE AND PRINT EST                         
         BAS   RE,BILLDNR          DELETE AND PRINT BILLS                       
         BAS   RE,STABDNR          DELETE AND PRINT MANUAL BILLS                
         BAS   RE,UNTDNR           DELETE AND PRINT UNITS/PACKAGES              
         GOTO1 AGOALDNR,DMCB,(RC)  DELETE AND PRINT GOALS                       
         MVI   ALLOWLIN,X'0C'                                                   
         BAS   RE,SPOOLIT                                                       
         GOTO1 ABRKTOT,DMCB,(RC)                                                
         BAS   RE,CLEARTOT                                                      
         MVI   FORCEHED,C'Y'                                                    
                                                                                
* ADD CLIENTS TO CLSDT LIST FOR I8/TR CLOSEOUT REQUESTS                         
         CLI   NBSELEST,0          ALL ESTIMATES?                               
         BNE   PROCDXX             NO-DON'T DO I8/TR REQUESTS                   
*                                                                               
*                                                                               
PDNR01   DS    0H                                                               
         L     R1,=A(CLTTBL)              CLOSED CLIENT LIST                    
PDNR05   CLI   0(R1),0                                                          
         BE    PDNR10                                                           
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INCREASE TABLE SIZE                          
         CLC   0(3,R1),ACTVCLT     ALREADY PASSED CLIENT                        
         BE    PROCDXX             YES-THAT'S ALL                               
         LA    R1,3(R1)                                                         
         B     PDNR05                                                           
PDNR10   MVC   0(3,R1),ACTVCLT     SET CLIENT TO TABLE                          
*                                                                               
PROCDXX  B    EXIT                                                              
         SPACE                                                                  
*                                                                               
GETSPC   NTR1              ADD SPECIALS TO NETVALUE BILLING/PAYING              
         L     R2,NBAIO                                                         
         USING NUBILD,R2                                                        
         MVI   ELCODE,X'10'          BILLING                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SPC10    BAS   RE,NEXTEL                                                        
         BNE   SPC20                                                            
         CLI   NUBILTYP,C'T'          SKIP TIME(NETVALUE GETS IT)               
         BE    SPC10                                                            
         CLI   NUBILTYP,C'I'          SKIP INTEGRATION                          
         BE    SPC10                                                            
         ICM   R1,15,NUBILGRS                                                   
         ICM   R3,15,NBBILTGR                                                   
         AR    R3,R1                                                            
         STCM  R3,15,NBBILTGR                                                   
         B     SPC10                                                            
*                                                                               
SPC20    L     R2,NBAIO                                                         
         MVI   ELCODE,X'12'              PAYING                                 
         USING NUPAYD,R2                                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SPC30    BAS   RE,NEXTEL                                                        
         BNE   SPCX                                                             
         CLI   NUPAYTYP,C'T'       SKIP TIME                                    
         BE    SPC30                                                            
         CLI   NUPAYTYP,C'I'       AND INTEGRATION                              
         BE    SPC30                                                            
         ICM   R1,15,NUPAYGRS                                                   
         ICM   R3,15,NBPAYTGR                                                   
         AR    R3,R1                                                            
         STCM  R3,15,NBPAYTGR                                                   
         B     SPC30                                                            
SPCX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
********************************************                                    
* ROUTINE READS BILLING RECORDS                                                 
* ADDS BILLREC DOLLARS TO BILLREC TOTS                                          
*                                                                               
GETBILL  NTR1                                                                   
         BAS   RE,BILLHI                                                        
         BE    GB10                                                             
GB5      BAS   RE,BILLSEQ                                                       
         BNE   GBX                                                              
GB10     MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOGET                                                         
*        GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         L     R2,AIO                                                           
         USING BILLREC,R2                                                       
         PACK  WORK(10),BAMT                                                    
         AP    BLRECAMT,WORK(10)           ADD TO BILL REC TOTALS               
GB15     L     R1,BILLCNTR                                                      
         LA    R1,1(R1)                                                         
         ST    R1,BILLCNTR                                                      
         B     GB5                                                              
*                                                                               
GBX      CLI   NBMODE,NBVALCLI                                                  
***      BE    GBXX                                                             
         NETGO NVSETUNT,DMCB                                                    
         MVI   NBFUNCT,NBFRDHI                                                  
         XC    FILENAME,FILENAME                                                
GBXX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
* READS BUCKET RECORDS - USED FOR MANUAL BILLING IN NETWORK                     
*       KEEPS TOTAL                                                             
*                                                                               
GETMAN   NTR1                                                                   
         BAS   RE,STABHI                                                        
         BNE   GT5                                                              
         B     GT7                                                              
GT5      BAS   RE,STABSEQ                                                       
         BNE   GTX                                                              
GT7      DS    0H                                                               
         MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOGET                                                         
         L     R1,STABCNTR                                                      
         LA    R1,1(R1)                                                         
         ST    R1,STABCNTR                                                      
         L     R2,AIO                                                           
         LA    R2,24(R2)                                                        
GT15     CLI   0(R2),X'0E'         GET ELEMENT                                  
         BNE   GT17                                                             
         USING STABELEM,R2                                                      
         ICM   R1,15,STABGRS                                                    
         CVD   R1,DUB                                                           
         AP    STABAMT,DUB                                                      
GT17     CLI   1(R2),0             ARE THERE MULTIPLE ELEMS                     
         BE    GT5                                                              
         ZIC   R1,1(R2)            ARE THERE MULTIPLE ELEMS                     
         AR    R2,R1                                                            
         B     GT15                                                             
GTX      DS    0H                                                               
         BAS   R5,RESET                                                         
GTXX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************                      
*     UNIT BILLED=UNIT PAID                                                     
*                                                                               
UNTBLPD  NTR1                                                                   
         MVI   ERROR,0             UNIT BILL/PAID ERROR                         
* UNIT PAID DOLS = UNIT BILLED DOLS                                             
         L     R1,NBBILTGR                                                      
         A     R1,NBBILTNT                                                      
         L     R2,NBPAYTGR                                                      
         A     R2,NBPAYTNT                                                      
         CR    R1,R2                                                            
         BE    *+8                                                              
         BNE   UBPERR                                                           
* - CHECK NET BILLED=NET PAID (ALL TYPES)                                       
         SR    R3,R3                                                            
         SR    R4,R4                                                            
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'12'                                                     
         USING NUPAYEL,R2                                                       
         BAS   RE,GETEL                                                         
         BNE   UBP10                                                            
         MVC   FULL,NUPAYNET                                                    
         A     R3,FULL                                                          
         BAS   RE,NEXTEL                                                        
         BE    *-14                                                             
UBP10    L     R2,NBAIO                                                         
         MVI   ELCODE,X'10'                                                     
         USING NUBILEL,R2                                                       
         BAS   RE,GETEL                                                         
         BNE   UBP15                                                            
UBP11    TM    NUBILST,X'20'       IF UNBILLED                                  
         BNZ   UBP12                                                            
         MVC   FULL,NUBILNET       SKIP IT                                      
         A     R4,FULL                                                          
UBP12    BAS   RE,NEXTEL                                                        
         BE    UBP11                                                            
UBP15    CR    R3,R4                                                            
         BNE   UBP20                                                            
         C     R3,=F'0'            IF ZERO                                      
         BE    UBPX                SKIP RATETYPE/NETNET TEST                    
         B     UBPX                SKIP THIS RATETYPE CHECK                     
**                                GIVES ERRORS IF RATETYPE CHANGED              
         MVC   BYTE,NBRTTYPE                                                    
         L     R1,NBACTUAL         TOTAL GROSS=ACTUAL                           
         L     R5,NBINTEG                +INTEGRATION                           
         AR    R1,R5                                                            
         ICM   R5,15,NBSPCHRG               +SPECIAL                            
         AR    R1,R5                                                            
         ST    R1,FULL                                                          
         GOTO1 =V(NETNET),DMCB,(BYTE,FULL),DUB                                  
         L     R1,DUB+4                                                         
         CR    R3,R1                                                            
         BE    UBPX                                                             
         B     UBP20                                                            
*                                                                               
UBP20    MVI   ERROR,3                                                          
*****    STCM  R4,15,WORK+20       PASS BILLED NET                              
*****    STCM  R3,15,WORK+24       PASS PAID NET                                
UBPERR   GOTO1 AERRORS,DMCB,(RC)                                                
UBPX     B     EXIT                                                             
         EJECT                                                                  
*****************************************                                       
*  DELETES AND PRINTS ESTIMATE                                                  
* ESTIMATE KEY IS IN ESTKEYSV                                                   
* WRITE OUT ESTIMATE WITH APPROPRIATE DATA                                      
* DELETE DIRECTORY AND FILE                                                     
* FOR FINAL SUMMARY SET FIRST EST REC TO SUM SAVE AREA                          
*                                                                               
ESTDNR   NTR1                                                                   
         MVI   ESTADD,0                                                         
         MVI   SWIT,C'E'                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   AIO,ANETWS1        SET I/O AREA FOR SPOT RECORDS                 
         L     R3,AIO                                                           
         USING ESTHDR,R3                                                        
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVC   KEY,ESTKEYSV                                                     
         CLC   =C'POL',SPLPRO      IS IT POL                                    
         BNE   *+10                                                             
         XC    KEY+4(3),KEY+4      YES/CLEAR PROD FIELD                         
         MVC   FILENAME,=C'SPTDIR  '                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     EST5                                                             
ESTSEQ   MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
EST5     CLC   =C'POL',SPLPRO              IS IT POL                            
         BE    EST7                                                             
         CLC   KEY(13),KEYSAVE      NO/TEST ENTIRE KEY                          
         BNE   ESTX                                                             
         B     EST10                                                            
EST7     CLC   KEY(4),KEYSAVE       YES/DONOT TEST PROD   ID/AM/CLT             
         BNE   ESTX                                                             
         CLC   KEY+7(5),KEYSAVE+7       ESTIMATE+REST(0S)                       
         BNE   ESTSEQ           SINCE ON RDHI I WONT GET IT 1ST TIME            
EST10    MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOGET                                                         
*        GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         CLI   DELETESW,C'Y'                                                    
         BNE   ESTWRT                                                           
****     L     R1,AIO                                                           
****     USING ESTHDR,R1                                                        
         TM    EPRDCD,X'80'        IS IT NEW NETWORK EST                        
         BNO   ESTWRT              IF NO/DO NOT CLOSE OUT ESTHDR                
         L     R1,AIO                                                           
         OI    15(R1),X'C0'                                                     
         BAS   RE,IOPUT                                                         
*        GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         OI    KEY+13,X'C0'                                                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   RE,WRTDIR           WRITE TO DIRECTORY                           
ESTWRT   DS    0H                                                               
         LA    R2,P                                                             
         USING BPLINED,R2                                                       
***      XC    0(132,R2),0(R2) FOR SUMMARY P LINE NOT CLEAR                     
         BAS   RE,CLEARP                                                        
*                              SINCE IT IS NOT PRINTED                          
         GOTO1 =V(CLUNPK),DMCB,EKEYCLT,BPCLT                                    
         MVC   BPPRD,EKEYPRD                                                    
         EDIT  (B1,EKEYEST),(3,BPEST)                                           
**       MVC   BPYRSRV(6),ESTART                                                
         GOTO1 DATCON,DMCB,ESTART,(X'20',BPYRSRV)                               
         MVI   BPYRSRV+6,C'-'                                                   
**       MVC   BPYRSRV+7(6),EEND                                                
         GOTO1 DATCON,DMCB,EEND,(X'20',BPYRSRV+7)                               
         DROP  R2                                                               
         CLI   ESTWARN,0           IS THERE A WARNING ON                        
         BE    EST15                                                            
         CLI   ESTWARN,3                                                        
         BNE   *+14                                                             
         MVC   36(24,R2),=C'*** NET/BILLED ERROR ***'                           
         B     *+10                                                             
         MVC   36(24,R2),=C'*** WARNING - ERRORS ***'                           
         MVI   P+29,1           SET WARN FOR SUMMARY PRINT                      
         CLI   ESTWARN,3                                                        
         BNE   *+8                                                              
         MVI   P+29,3                                                           
         MVI   ESTWARN,0           CLEAR WARN SWITCH                            
EST15    DS    0H                                                               
         CLI   ESTADD,1          HAS EST REC BEEN ADDED FOR SUMMARY             
         BE    EST35                                                            
         MVI   ESTADD,1                                                         
         L     R2,ASUMRECD       MOVE EST DATA TO SUMMARY SAVE AREA             
         L     R1,SUMCNTR                                                       
         C     R1,=F'300'         MAX NUMBER OF RECS                            
         BL    *+6                                                              
         DC    H'0'                                                             
         MH    R1,=H'100'                                                       
         AR    R2,R1                                                            
         MVC   0(100,R2),SPACES                                                 
         MVC   0(30,R2),P          SAVE THE ESTIMATE DATA                       
         ST    R2,ADRSUMRC         SET ADRS FOR LATER $ STORE                   
         L     R1,SUMCNTR          BUMP SUM REC COUNTER                         
         LA    R1,1(R1)                                                         
         ST    R1,SUMCNTR                                                       
*                                                                               
EST35    BAS   RE,SPOOLIT                                                       
***      XC    P,P                 FOR S OPT NO WRITE/NO CLEAR P                
         BAS   RE,CLEARP                                                        
         L     R1,ESTCNTR                                                       
         LA    R1,1(R1)                                                         
         ST    R1,ESTCNTR                                                       
         CLC   =C'POL',SPLPRO      IT IT POL REQUEST                            
         BE    ESTSEQ                                                           
ESTX     DS    0H                                                               
         BAS   R5,RESET                                                         
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*******************************************                                     
* DELETE AND PRINT BILL RECORDS                                                 
* READ BILL RECS AND DELETE DIRECTORY AND FILE                                  
* PRINT OUT APPROPRIATE DATA                                                    
*                                                                               
BILLDNR  NTR1                                                                   
         MVI   SWIT,C'B'                                                        
         MVI   ESTADD,0         UES ESTADD AS SWITCH FOR HEAD PRINT             
         BAS   RE,SPOOLIT                                                       
         BAS   RE,BILLHI                                                        
         L     R2,AIO                                                           
         USING BILLREC,R2                                                       
         BE    BILL7                                                            
*                                                                               
**       XC    P,P                                                              
         BAS   RE,CLEARP                                                        
         MVC   P(18),=C'NO BILLING RECORDS'                                     
         BAS   RE,SPOOLIT                                                       
         B     BILLX                                                            
BILL5    BAS   RE,BILLSEQ                                                       
         BNE   BILLX                                                            
BILL7    MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOGET                                                         
*        GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         CLI   DELETESW,C'Y'                                                    
         BNE   BILLWRIT                                                         
         OI    BCNTRL,X'C0'                                                     
         BAS   RE,IOPUT                                                         
*        GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         OI    KEY+13,X'C0'                                                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   RE,WRTDIR              WRITES TO DIRECTORY                       
BILLWRIT LA    R3,P                                                             
         USING BPLINED,R3                                                       
         GOTO1 =V(CLUNPK),DMCB,BKEYCLT,BPCLT      CLIENT                        
         MVC   BPPRD,BKEYPRD                  PRODUCT                           
         EDIT  (B1,BKEYEST),(3,BPEST)          ESTIMATE                         
         EDIT  (B1,BKEYYSRV),(2,BPYRSRV)       YEAR SERVICE                     
         MVI   BPMNSRV+2,C'/'                                                   
         EDIT  (B1,BKEYMSRV),(2,BPMNSRV)        MONTH SERVICE                   
*        MVC   BYTE,BKEYMBIL                                                    
*        NI    BYTE,X'0F'                                                       
*        EDIT  (B1,BYTE),(1,BPMNYR)                                             
*        MVC   BPMNYR+1(2),=C'/8'                                               
*        ZIC   R1,BKEYMBIL                                                      
*        SRA   R1,4                                                             
*        LA    R4,BPMNYR+3                                                      
*        EDIT  (R1),(1,(R4))                                                    
*        EDIT  (B2,BKEYINV),(4,BPNUM)          BILL NUMBER                      
         MVC   BPINVO,BINVNO                   BILL INVOICE                     
         MVC   BPDATE,BDATE                    BILL DATE                        
*        EDIT  (C10,BAMT),(11,BPAMT),2,CR=YES BILL AMOUNT                       
         EDIT  (C10,BAMT),(13,BPAMT),2,MINUS=YES  BILL AMOUNT                   
         CLI   ESTADD,1            HAS HEADING BEEN PRINTED                     
         BE    BILL12                                                           
         MVI   ESTADD,1                                                         
         BAS   RE,BILLHEAD                                                      
BILL12   BAS   RE,SPOOLIT                                                       
         PACK  WORK(10),BAMT             ADD TO BLRECAM2                        
         AP    BLRECAM2,WORK(10)                                                
         L     R1,BILLCNT2               ADD TO BILL COUNTER                    
         LA    R1,1(R1)                                                         
         ST    R1,BILLCNT2                                                      
         B     BILL5                                                            
*                                                                               
BILLX    MVI   SWIT,0                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*  WRITE AND DELET STAB RECORDS                                                 
*                                                                               
STABDNR  NTR1                                                                   
         BAS   RE,SPOOLIT                                                       
         MVI   SWIT,C'S'                                                        
         MVI   ESTADD,0            USE FOR HEADING PRINT SWITCH                 
                                                                                
*  CLOSE OUT NEW 3 CHARACTER PRODUCT PASSIVE POINTERS                           
         BRAS  RE,STABPASV                                                      
                                                                                
*   CLOSE ACTIVE KEY AND RECORD                                                 
         BAS   RE,STABHI                                                        
         L     R2,AIO                                                           
         USING STABUCK,R2                                                       
         BNE   STABX                                                            
         B     STAB7                                                            
STAB5    BAS   RE,STABSEQ                                                       
         BNE   STABX                                                            
STAB7    MVC   FILENAME,=C'SPTFIL  '                                            
         BAS   RE,IOGET                                                         
*        GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         CLI   DELETESW,C'Y'                                                    
         BNE   STABWRIT                                                         
         OI    STABCNTL,X'C0'                                                   
         BAS   RE,IOPUT                                                         
*        GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         OI    KEY+13,X'C0'                                                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         BAS   RE,WRTDIR               WRITE TO DIRECTORY                       
*                                                                               
STABWRIT LA    R3,P                                                             
         USING BPLINED,R3                                                       
         GOTO1 =V(CLUNPK),DMCB,STABKCLT,BPCLT                                   
         MVC   WORK(1),STABKPRD                                                 
         BAS   RE,GETPRD                                                        
         MVC   BPPRD,WORK+10                                                    
         EDIT  (B1,STABKEST),(3,BPEST)                                          
****     GOTO1 AMSUNPK,DMCB,STABKMKT,BPMNSRV,BPYRSRV                            
         EDIT  (B2,STABINV),(6,BPINVO)                                          
         GOTO1 DATCON,DMCB,(2,STABBDT),(X'20',BPDATE)                           
         LA    R5,STABELEM                                                      
         USING STABELEM,R5                                                      
         SR    R1,R1               R1 = TOTAL BUCKET                            
STAB10   CLI   0(R5),X'0E'                                                      
***      BNE   STAB11                                                           
***      L     R0,STABGRS                                                       
***      AR    R1,R0               SAVE TOTAL IN R1                             
         BNE   STAB10B                                                          
         ICM   R1,15,STABGRS                                                    
         CVD   R1,DUB                                                           
         AP    STABAMT2,DUB                                                     
STAB10B  ZIC   R0,1(R5)                                                         
         CLI   1(R5),0                                                          
         BE    STAB11                                                           
         AR    R5,R0                                                            
         B     STAB10                                                           
STAB11   DS    0H                                                               
****     CVD   R1,DUB                                                           
****     AP    STABAMT2,DUB        ADD TO TOTALS                                
         ZAP   DUB,STABAMT2                                                     
         CVB   R1,DUB                                                           
         EDIT  (R1),(10,BPAMT),2                                                
         CLI   ESTADD,1                   HAS HEADING BEEN PRITNED              
         BE    STAB12                                                           
         MVI   ESTADD,1                                                         
         BAS   RE,STABHEAD                                                      
STAB12   BAS   RE,SPOOLIT                                                       
         L     R1,STABCNT2                ADD TO MANUAL BILL CNTR               
         LA    R1,1(R1)                                                         
         ST    R1,STABCNT2                                                      
         B     STAB5                                                            
*                                                                               
STABX    DS    0H                                                               
         BAS   R5,RESET                                                         
         B     EXIT                                                             
         DROP  R2,R3,R5                                                         
         SPACE 2                                                                
*                                                                               
RESET    DS    0H                                                               
         MVC   AIO,NBAIO         RESET NETIO I/O AREA                           
         NETGO NVSETUNT,DMCB                                                    
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         MVI   SWIT,0                                                           
         BR    R5                                                               
         EJECT                                                                  
*******************************************                                     
* DELETE AND PRINT PACKAGE/UNIT RECORDS                                         
* SAVE OLD NETBLOCK                                                             
* HAVE NETIO READ PACKAGE RECORDS                                               
* USE DISK ADDRESSES TO REREAD UNITS                                            
* PRINT OUT APPROPRIATE DATA                                                    
* FINAL SUMMARY UNTCNTR/TOTALS                                                  
*                                                                               
UNTDNR   NTR1                                                                   
*                                                                               
DOPKG    MVC   P(15),=C'PACKAGE RECORDS'                                        
         BAS   RE,SPOOLIT                                                       
*                                                                               
         NETGO NVSETUNT,DMCB                                                    
         MVC   FILENAME,=C'UNTDIR  '                                            
         MVC   AIO,ANETWS1                                                      
         LA    R2,KEY                                                           
         USING NPKEY,R2                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'           SET PKG TYPE                                 
         MVC   NPKAM,ESTKEYSV+1    A/M                                          
         MVC   NPKCLT,ESTKEYSV+2   CLIENT                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     DOP5                                                             
PKGSEQ   DS    0H                                                               
         MVC   FILENAME,=C'UNTDIR  '                                            
         GOTO1 SEQ                                                              
DOP5     CLC   KEY(14),KEYSAVE     TYPE/AM/CLT                                  
         BNE   DOUNT                   NOT EQUAL/DO UNITS                       
         CLC   NPKEST,ESTKEYSV+7   EST                                          
         BNE   PKGSEQ                                                           
         MVC   FILENAME,=C'UNTFIL  '                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+21,AIO,(0,DMWORK)           
         CLI   DELETESW,C'Y'                                                    
         BNE   DOP10                                                            
         DROP  R2                                                               
*                                                                               
         L     R1,AIO                                                           
         USING NPRECD,R1                                                        
         OI    NPKRSTAT,X'C0'      CLOSE OUT                                    
         DROP  R1                                                               
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+21,AIO,(0,DMWORK)           
         OI    KEY+20,X'C0'                                                     
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,WRTDIR                                                        
*                                                                               
DOP10    DS    0H                                                               
         L     R3,AIO                                                           
         USING NPRECD,R3                                                        
         XC    DMCB,DMCB                                                        
         GOTO1 =V(CLUNPK),DMCB,NPKCLT,P+1                                       
         MVC   P+5(4),NPKNET                                                    
         EDIT  (B1,NPKEST),(4,P+10)                                             
         EDIT  (B1,NPKPACK),(4,P+15)                                            
         MVC   P+20(16),NPAKNAME                                                
         BAS   RE,SPOOLIT                                                       
         AF    PKGCNTR,=F'1'                                                    
         B     PKGSEQ                                                           
         DROP  R3                                                               
*                                                                               
DOUNT    DS    0H                     **** PROCESS UNITS ****                   
         L     RF,ANETWS1                  SAVE NETBLOCK                        
         LA    RE,NETBLOCK                                                      
         LA    R1,NBBLKEND-NETBLOCK                                             
         MOVE  ((RF),(R1)),(RE)                                                 
         XC    ACTVCLT,ACTVCLT     CLEAR CLIENT CODE                            
*                                                                               
         MVI   BPRDSV,0            CLEAR PRODUCT SAVE                           
         MVI   SWIT,C'U'                                                        
         MVI   ESTADD,0            USE AS HEADING PRINT SWITCH                  
         BAS   RE,SPOOLIT                                                       
         XC    NBABILRD,NBABILRD   RESET BILLRD SAVE AREA                       
         L     R2,ADISKSV          GET DISK ADDR SAVE AREA                      
         L     R3,UNTCNTR          R3 = NUM OF UNITS READ FOR BCT               
         LTR   R3,R3                                                            
         BZ    DOUNTX                                                           
GETUNT   MVC   FILENAME,=C'UNTFIL  '                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,0(R2),NBAIO,(0,DMWORK)          
         MVI   NBFUNCT,NBFVAL                                                   
         NETGO NSNETIO,DMCB,NETBLOCK    HAVE NETIO FILL IN NETBLOCK             
         CLI   ACTVCLT,0           HAVE ACTVAL ?                                
         BNE   DNT05                                                            
         L     R1,NBAIO            NO-GET ACTVAL                                
         MVC   WORK(3),1(R1)       AGY/CLT                                      
         BRAS  RE,RDCLT          GET PRINTABLE CLIENT CODE INTO ACTVCL          
DNT05    BAS   RE,GETSPC                                                        
         BAS   RE,UNTHOOK                                                       
         CLI   DELETESW,C'Y'                                                    
         BE    UNTCLOSE                                                         
         BRAS  RE,CNTUNTBL         COUNT UNTI BILL RECS                         
UNTSEQ   LA    R2,5(R2)              BUMP TO NEXT SAVED UNIT DSKAD              
         BCT   R3,GETUNT                                                        
         B     DOUNTX                NO MORE UNITS/END                          
*                                                                               
DOUNTX   DS    0H                                                               
         L     RE,ANETWS1              RESET NETBLOCK                           
         LA    RF,NETBLOCK                                                      
         LA    R1,NBBLKEND-NETBLOCK                                             
         MOVE  ((RF),(R1)),(RE)                                                 
***      B     DNT10               *** READING REC CAN CAUSE DUMP               
***                                    IF WE DO NOT HAVE A UNIT IN I/O          
***                                    E.G. CLI HEADER                          
         CLI   NBKEY,X'94'         IS IT UNIT PASSIVE KEY                       
         BNE   DNT10                                                            
         MVC   FILENAME,=C'UNTDIR  ' PUT CURRENT UNIT BACK                      
         MVC   KEY,NBKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'UNTFIL  '                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+21,NBAIO,(0,DMWORK)         
DNT10    MVI   NBFUNCT,NBFRDHI                                                  
         MVI   SWIT,0                                                           
         B     EXIT                                                             
         EJECT                                                                  
***  WARNING - DO NOT USE R2/R3 IN BELOW ROUTINE ***                            
***            R2 POINTS TO UNIT DISK ADDRESS IN SAVE AREA                      
***            R3 = BCT LIMIT FOR NUMBER OF SAVED DISK ADDRESSES                
*                                                                               
UNTCLOSE DS    0H               *** CLOSE OUT UNIT                              
         MVC   FILENAME,=C'UNTFIL  '  READ REC AGAIN WITHOUT                    
*                                     GOING THROUGH BILLRDR                     
*                                     TO PRESERVE X'10' ON UNIT                 
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,0(R2),NBAIO,(0,DMWORK)          
         L     R1,NBAIO                                                         
         MVC   UNITKSV,0(R1)       SAVE KEY FOR NEW BILLREC                     
         OI    22(R1),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,0(R2),NBAIO,(0,DMWORK)          
*                                                                               
         XC    KEY,KEY              ACTIVE KEY                                  
         L     R1,NBAIO                                                         
         MVC   KEY(20),0(R1)        READ IT AND TEST TO BE SURE                 
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'UNTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+21(4),0(R2)     ALSO TEST DSKAD AS EXTRA CHECK               
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+20,X'C0'                                                     
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,WRTDIR                                                        
*                                                                               
         LA    R1,KEY              PASSIVE KEY                                  
         USING NUKPKEY,R1                                                       
         XC    KEY+4(16),KEY+4                                                  
         MVI   KEY,X'84'                                                        
         MVC   NUKPNET,NBACTNET                                                 
         MVC   NUKPPROG,NBACTPRG                                                
         MVC   NUKPDATE,NBACTDAT                                                
         MVC   NUKPEST,NBACTEST                                                 
         MVC   NUKPSUB,NBACTSUB                                                 
         MVC   NUKPDP,NBACTDP                                                   
         OI    KEY+20,X'C0'                                                     
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,WRTDIR                                                        
*                                                                               
         USING NUKDKEY,R1            SECOND PASSIVE KEY                         
         XC    KEY+4(16),KEY+4                                                  
         MVI   KEY,X'94'                                                        
         MVC   NUKDEST,NBACTEST                                                 
         MVC   NUKDNET,NBACTNET                                                 
         BAS   RE,DAYLOOK           PUTS ONE BYTE DAY CODE IN NUKDDAY           
         MVC   NUKDTIME,NBACTSQH                                                
         MVC   NUKDPROG,NBACTPRG                                                
         MVC   NUKDDATE,NBACTDAT                                                
         MVC   NUKDSUB,NBACTSUB                                                 
         OI    KEY+20,X'C0'                                                     
         MVC   FILENAME,=C'UNTDIR  '                                            
         BAS   RE,WRTDIR                                                        
         B     DOHIST                                                           
*                                                                               
DOHIST   GOTO1 =A(HISTRY),DMCB,(RC)                                             
*                                                                               
         BRAS   RE,BILLRC          CLOSE OUT NEW UNIT BILL REC                  
*                                                                               
UNTCLX   B     UNTSEQ              RETURN FOR NEXT UNIT                         
         EJECT                                                                  
*                                                                               
DAYLOOK  DS    0H                                                               
         L     R4,=A(DAYLKUP)                                                   
DLK2     CLC   0(3,R4),NBDAYNAM                                                 
         BE    DLK5                                                             
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   DLK2                                                             
         DC    H'0'                                                             
DLK5     MVC   NUKDDAY,3(R4)                                                    
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
UNTHOOK  NTR1                  NETIO HOOK FOR UNIT RECORDS                      
         LA    R3,P                                                             
         USING UPLINED,R3                                                       
         CLI   4(R2),0             DOES UNIT HAVE WARNING                       
         BE    *+8                                                              
         MVI   P+131,C'X'                                                       
         CLI   4(R2),C'B'          IS IT CURRENT BILL MONTH ERR                 
         BNE   *+8                                                              
         MVI   P+131,C'C'                                                       
         GOTO1 NBCLUNPK,DMCB,NBACTCLI,UPCLT  CLIENT                             
         CLC   NBPRD,BPRDSV                                                     
         BE    DNR5                                                             
         MVC   WORK(1),NBPRD                                                    
         GOTO1 GETPRD                                                           
         MVC   BPRDSV,NBPRD        SET NEW BPRD                                 
         MVC   PRDSV,WORK+10       SET NEW 3CL PRD                              
DNR5     MVC   UPPRD,PRDSV                   PRODUCT                            
         EDIT  (B1,NBPACK),(3,UPPAK)         PACKAGE                            
         MVC   UPNET,NBACTNET                NETWORK                            
         MVC   UPPROG,NBACTPRG               PROGRAM                            
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,UPDATE)   DATE                       
         MVI   UPDATE+8,C'-'                                                    
         EDIT  (B1,NBACTSUB),(2,UPSUBLN)              SUB-LINE                  
         TM    NBUNITST,X'42'      TEST MISSED/PRE-EMPT                         
         BNZ   DNR5A                YES/SKIP ACTUAL                             
         L     R1,NBACTUAL                                                      
         ICM   RE,15,NBSPCHRG                                                   
         AR    R1,RE                                                            
         BAS   RE,XCENT                                                         
         AP    UNTORD2,DUB                                                      
         LA    R4,UPACTUAL                                                      
         BAS   R5,EDITR1                                                        
****     EDIT  (R1),(12,UPACTUAL),2,MINUS=YES                                   
         L     R1,NBINTEG                                                       
         BAS   RE,XCENT                                                         
         AP    UNTORD2,DUB                                                      
         LA    R4,UPINTEG                                                       
         BAS   R5,EDITR1                                                        
***      EDIT  (R1),(12,UPINTEG),2,MINUS=YES                                    
*                                                                               
DNR5A    L     R1,NBBILTGR         BILLED TIME                                  
         BAS   RE,XCENT                                                         
         AP    UNTBILL2,DUB                                                     
         LA    R4,UPBILT                                                        
         BAS   R5,EDITR1                                                        
****     EDIT  (R1),(12,UPBILT),2,MINUS=YES                                     
         L     R1,NBBILIGR              BILLED INTEG                            
         BAS   RE,XCENT                                                         
         AP    UNTBILL2,DUB          BILLED TIME + INTEG =BILLED UNIT           
         LA    R4,UPBILI                                                        
         BAS   R5,EDITR1                                                        
****     EDIT  (R1),(12,UPBILI),2,MINUS=YES                                     
*                                                                               
         L     R1,NBPAYTGR              PAID TIME                               
         BAS   RE,XCENT                                                         
         AP    UNTPAID2,DUB                                                     
         LA    R4,UPPAIDT                                                       
         BAS   R5,EDITR1                                                        
*****    EDIT  (R1),(12,UPPAIDT),2,MINUS=YES                                    
         L     R1,NBPAYIGR              PAID INTEG                              
         BAS   RE,XCENT                                                         
         AP    UNTPAID2,DUB          PAID TIME + INTEG =PAID UNIT               
         LA    R4,UPPAIDI                                                       
         BAS   R5,EDITR1                                                        
****     EDIT  (R1),(12,UPPAIDI),2,MINUS=YES                                    
*                                                                               
         TM    NBUNITST,X'42'      TEST MISSED/PRE-EMPT                         
         BNZ   DNR5B                YES/SKIP ASSIGNED                           
         L     R1,NBASSIGN             ASSIGNED                                 
         CLC   =C'OM',NBSELAGY     FOR OGILVY ADD INTG/SPEC                     
         BNE   SKIP10              TO ASSIGNED                                  
         L     RE,NBINTEG                                                       
         AR    R1,RE                                                            
         ICM   RE,15,NBSPCHRG                                                   
         AR    R1,RE                                                            
SKIP10   BAS   RE,XCENT                                                         
         AP    UNTASSG2,DUB                                                     
         TM    NBUNITST,X'42'      TEST MISSED/PRE-EMPT                         
         BNZ   DNR5B                YES/DONT PRINT ASSIGNED                     
         LA    R4,UPASSGN                                                       
         BAS   R5,EDITR1                                                        
*****    EDIT  (R1),(12,UPASSGN),2,MINUS=YES                                    
*                                                                               
DNR5B    SR    R3,R3              ADD NET PAID TO CLIENT TOTS                   
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'12'                                                     
         USING NUPAYEL,R2                                                       
         BAS   RE,GETEL                                                         
         BNE   NET100                                                           
         MVC   FULL,NUPAYNET                                                    
         A     R3,FULL                                                          
         BAS   RE,NEXTEL                                                        
         BE    *-14                                                             
         CVD   R3,DUB                                                           
         AP    TOTNETP,DUB                                                      
NET100   SR    R3,R3              ADD NET BILLED TO CLIENT TOTS                 
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'10'                                                     
         USING NUBILEL,R2                                                       
         BAS   RE,GETEL                                                         
         BNE   NET101                                                           
NET100B  TM    NUBILST,X'20'       REVERSED/UNBILLED?                           
         BNZ   NET100C             YES/SKIP                                     
         MVC   FULL,NUBILNET                                                    
         A     R3,FULL                                                          
NET100C  BAS   RE,NEXTEL                                                        
         BE    NET100B                                                          
NET101   CVD   R3,DUB                                                           
         AP    TOTNETB,DUB                                                      
*                                                                               
         CLI   ESTADD,1             HAS HEADING BEEN PRINTED                    
         BE    DNR7                                                             
         MVI   ESTADD,1                                                         
         BRAS  RE,UNTHEAD                                                       
DNR7     BAS   RE,SPOOLIT                                                       
         L     R1,UNTCNTR2                                                      
         LA    R1,1(R1)                                                         
         ST    R1,UNTCNTR2                                                      
DNRUNTX  B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
EDITR1   EDIT  (R1),(12,0(R4)),2,MINUS=YES                                      
         BR    R5                                                               
*                                                                               
         EJECT                                                                  
*******************************************                                     
* BILL REC READING                                                              
* EXCPECTS KEY INFO IN ESTYKEYSV                                                
*                                                                               
BILLHI   NTR1                                                                   
         MVC   AIO,ANETWS1                                                      
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         XC    KEY,KEY                                                          
         MVC   KEY,ESTKEYSV                                                     
         CLC   =C'POL',SPLPRO      IS IT POL                                    
         BNE   *+10                                                             
         XC    KEY+4(3),KEY+4      YES/CLEAR PROD FIELD                         
         MVI   KEY+8,1         SET TO 1 TO DISTINGUISH FROM EST KEY             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     BH7                                                              
BILLSEQ  NTR1                                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
BH6      GOTO1 SEQ                                                              
BH7      DS    0H                                                               
         CLC   =C'POL',SPLPRO        IS IT POL                                  
         BE    *+14                                                             
         CLC   KEY(8),KEYSAVE       NO/TEST ENTIRE KEY                          
         B     BHX                                                              
         CLC   KEY(4),KEYSAVE        YES/ ID/A-M/CLT                            
         BNE   BHX                                                              
         CLI   KEY+8,0             TEST FOR EST HEADER                          
         BE    BH6                 IF YES GET FOLLOWING BILL REC                
         CLC   KEY+7(1),KEYSAVE+7         ESTIMATE                              
         BNE   BH6                                                              
BHX      B     EXIT                   COND CODE IS SET                          
         EJECT                                                                  
*****************************************                                       
* READS STATION BUCKET RECORDS (STAB)                                           
* EXPECTS KEY IN ESTKEYSV                                                       
*                                                                               
STABHI   NTR1                                                                   
         MVC   AIO,ANETWS1                                                      
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(3),ESTKEYSV+1        AM/CLT                                
         MVC   KEY+6(1),ESTKEYSV+7        ESTIMATE                              
         CLC   =C'POL',SPLPRO                                                   
         BE    ST3                                                              
         MVC   WORK(3),ESTKEYSV+4       PASS 3 CHAR PRD                         
         BAS   RE,GETBPRD               RETURNS 1 BYTE PRD                      
         MVC   KEY+5(1),WORK            IN WORK                                 
ST3      MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     ST5                                                              
STABSEQ  NTR1                                                                   
         MVC   FILENAME,=C'SPTDIR  '                                            
ST4      GOTO1 SEQ                                                              
ST5      CLC   =C'POL',SPLPRO                                                   
         BE    ST7                                                              
         CLC   KEY(13),KEYSAVE     TEST ENTIRE KEY                              
         B     STX                                                              
ST7      CLC   KEY(5),KEYSAVE             ID/AM/CLT                             
         BNE   STX                                                              
         CLC   KEY+6(1),KEYSAVE+6           EST                                 
         BNE   ST4                                                              
STX      B     EXIT                SET COND CODE                                
         EJECT                                                                  
*****************************************************                           
* TEST IF ANY UNIT ELEM BILL MONTH = CURRENT MONTH                              
* TEST IF ANY UNIT ELEM PAY  MONTH = CURRENT MONTH                              
*      IF YES / SKIP THE ESTIMATE                                               
*                                                                               
*                                                                               
CURMNTH  NTR1                                                                   
         MVI   ERROR,1             CURRMNTH BILL ERROR                          
         GOTO1 DATCON,DMCB,(5,WORK),(0,WORK)   GET CURRENT DATE                 
         L     R2,NBAIO                                                         
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   CURX                                                             
         USING NUBILD,R2                                                        
         B     CUR7                                                             
CUR5     BAS   RE,NEXTEL                                                        
         BNE   CUR10                                                            
CUR7     GOTO1 DATCON,DMCB,(2,NUBILDAT),(0,WORK+6)                              
         CLC   WORK(4),WORK+6      BILDAT=CUR DAT                               
         BNE   CUR5                                                             
         GOTO1 AERRORS,DMCB,(RC)                                                
         B     CURX                                                             
CUR10    L     R2,NBAIO                                                         
         USING NUPAYD,R2                                                        
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   CURX                                                             
         B     CUR13                                                            
CUR12    BAS   RE,NEXTEL                                                        
         BNE   CURX                                                             
CUR13    GOTO1 DATCON,DMCB,(2,NUPAYDAT),(0,WORK+6)                              
         CLC   WORK(4),WORK+6      PAYDAT=CUR DAT                               
         BNE   CUR12                                                            
         GOTO1 AERRORS,DMCB,(RC)                                                
CURX     B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**************************************                                          
*     ADD TO UNIT TOTALS                                                        
*     ORDERED/PAID/BILLED                                                       
*                                                                               
ADDTOTS  NTR1                                                                   
         TM    NBUNITST,X'42'      CHK MISSED/PRE-EMPT                          
         BNZ   ADDTOT5             YES/SKIP ACTUAL                              
         L     R1,NBACTUAL              ORDERED                                 
         ICM   RE,15,NBSPCHRG                                                   
         AR    R1,RE                                                            
         BAS   RE,XCENT                                                         
         AP    UNTORD,DUB                                                       
         L     R1,NBINTEG               ADD INTEG TO ORDERED                    
         BAS   RE,XCENT                                                         
         AP    UNTORD,DUB                                                       
*                                                                               
ADDTOT5  DS    0H                       ASSIGNED                                
         TM    NBUNITST,X'42'      CHK MISSED/PRE-EMPT                          
         BNZ   ADDTOT6             YES/SKIP ASSIGNED                            
         L     R1,NBASSIGN                                                      
         CLC   =C'OM',NBSELAGY     FOR OGILVY ADD INTG/SPEC                     
         BNE   SKIP20              TO ASSIGNED                                  
         L     RE,NBINTEG                                                       
         AR    R1,RE                                                            
         ICM   RE,15,NBSPCHRG                                                   
         AR    R1,RE                                                            
SKIP20   BAS   RE,XCENT                                                         
         AP    UNTASSGN,DUB                                                     
*                                                                               
ADDTOT6  L     R1,NBBILTGR              BILLED TIME                             
         BAS   RE,XCENT                                                         
         AP    UNTBILL,DUB                                                      
         L     R1,NBBILIGR              BILLED INTEG                            
         BAS   RE,XCENT                                                         
         AP    UNTBILL,DUB          BILLED TIME + INTEG =BILLED UNIT            
*                                                                               
         L     R1,NBPAYTGR              PAID TIME                               
         BAS   RE,XCENT                                                         
         AP    UNTPAID,DUB                                                      
         L     R1,NBPAYIGR              PAID INTEG                              
         BAS   RE,XCENT                                                         
         AP    UNTPAID,DUB          PAID TIME + INTEG =PAID UNIT                
*                                                                               
         B     EXIT                                                             
         SPACE                                                                  
***************************************                                         
* ROUTINE CLEARS AND ZAPS TOTAL FIELDS                                          
*                                                                               
CLEARTOT NTR1                                                                   
         LA    R2,12               12=NUM OF TOT FIELDS                         
         LA    R3,UNTORD                                                        
CLR5     ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R2,CLR5                                                          
         LA    R2,7                7=NUM  OF CNTR FIELDS                        
         LA    R3,ESTCNTR                                                       
CLR7     XC    0(4,R3),0(R3)                                                    
         LA    R3,4(R3)                                                         
         BCT   R2,CLR7                                                          
         XC    PKGCNTR,PKGCNTR     ALSO CLEAR PKGCNTR                           
         B     EXIT                                                             
         SPACE                                                                  
*********************************                                               
* CLEAR RUN TOTAL FIELDS                                                        
CLRUNTOT NTR1                                                                   
         LA    R2,RUNTOTS                                                       
         LA    R3,22                                                            
TOT5     ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,TOT5                                                          
         B     EXIT                                                             
         SPACE                                                                  
********************************                                                
* EXPECTS VALUE IN R1                                                           
* RETURNS PACKED VALUE DIVIDED BY 100 IN DUB                                    
*                                                                               
         DS    F                                                                
XCENT    ST    RE,XCENT-4                                                       
*****    M     R0,=F'1'                                                         
****     D     R0,=F'100'          GET RID OF CENTS                             
         CVD   R1,DUB                                                           
         L     RE,XCENT-4                                                       
         BR    RE                                                               
         SPACE                                                                  
*****************************************                                       
* ROUTINE COMPARES BILLED TOTS=UNIT TOTS                                        
*                                                                               
UNTNBLS  NTR1                                                                   
         ZAP   WORK(8),=P'0'                                                    
         AP    WORK(8),STABAMT                                                  
         AP    WORK(8),BLRECAMT                                                 
         LA    R3,WORK                                                          
         CP    UNTBILL,WORK(8)                                                  
         BE    UNTX                                                             
         CLI   KILLSW,C'Y'                                                      
         BE    UNTNB3                                                           
         MVI   ERRORSW,2                                                        
         MVC   WORK(3),ESTKEYSV+1                                               
         BRAS  RE,CLTERRTB         ADD TO CLIENT ERROR TABLE                    
         MVC   P(45),=C'*** ERROR ***  BILLING NOT EQUAL UNIT BILLING'          
         GOTO1 NBCLUNPK,DMCB,ESTKEYSV+2,P2+7                                    
         MVC   P2(7),=C'CLIENT='                                                
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     UNTX                                                             
UNTNB3   MVI   ERROR,2                                                          
         GOTO1 AERRORS,DMCB,(RC)                                                
UNTX     B     EXIT                                                             
         SPACE                                                                  
**********************************                                              
* UNITS ORDERED=BILLED=PAID                                                     
* PAID ALWAYS MATCHES ACTUAL COST                                               
* BILLED = ACTUAL OR ASSIGNED DEPENDING ON PROFILE                              
*                                                                               
UNTOBP   NTR1                                                                   
         L     R2,NBACTUAL                                                      
         L     R3,NBINTEG                                                       
         AR    R2,R3                                                            
         ICM   R3,15,NBSPCHRG                                                   
         AR    R2,R3                                                            
         TM    NBUNITST,X'42'      CHK MISSED/PRE-EMPT                          
         BZ    *+6                 YES/CLEAR ORDERED                            
         SR    R2,R2                                                            
         L     R3,NBPAYTGR                                                      
         A     R3,NBPAYIGR                                                      
         CR    R2,R3                                                            
         BNE   OBPERR                                                           
         L     R2,NBACTUAL                                                      
         CLI   ASSIGNB,C'Y'        IS IT BILLED ON ASSIGNED                     
         BNE   UNTOB5                                                           
         L     R2,NBASSIGN         ASSIGNED BILLING                             
         LTR   R2,R2               IF NO ASSIGNED DOLLARS                       
         BNZ   *+8                    THEN USE ACTUAL                           
         L     R2,NBACTUAL                                                      
UNTOB5   L     R3,NBINTEG                                                       
         AR    R2,R3                                                            
         ICM   R3,15,NBSPCHRG                                                   
         AR    R2,R3                                                            
         TM    NBUNITST,X'42'      CHK MISSED/PRE-EMPT                          
         BZ    *+6                 YES/CLEAR ORDERED                            
         SR    R2,R2                                                            
         L     R3,NBBILTGR                                                      
         A     R3,NBBILIGR                                                      
         CR    R2,R3                                                            
         BE    OBPX                                                             
OBPERR   MVI   ERROR,0                                                          
         GOTO1 AERRORS,DMCB,(RC)                                                
OBPX     B     EXIT                                                             
         EJECT                                                                  
********************************************                                    
*  TO GET 3 CHAR PRD CODE FROM C LIST                                           
*  INPUT   WORK HAS PRDNO                                                       
*  OUTPUT  PRDCODE IN WORK+10                                                   
*                                                                               
GETPRD   NTR1                                                                   
         L     R2,ACLISTSV                                                      
GP10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GP12                                                             
         MVC   WORK+10(3),=C'UNA'   .SET TO UNDEFINED                           
         B     GPX                                                              
GP12     CLC   3(1,R2),WORK                                                     
         BE    GP14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GP10                RETURN TO LOOP                               
GP14     MVC   WORK+10(3),0(R2)      SET 3 CHAR PRINTABLE PRD CODE              
GPX      B     EXIT                                                             
         SPACE                                                                  
********************************************                                    
*  TO GET 1 CHAR PRD CODE FROM C LIST                                           
*  INPUT   WORK HAS 3 CHAR PRD CODE                                             
*  OUTPUT  BPRD IN WORK                                                         
*                                                                               
GETBPRD  NTR1                                                                   
         L     R2,ACLISTSV                                                      
GPB10    CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GPB12                                                            
         MVI   WORK,0               .SET TO UNDEFINED                           
         B     GPBX                                                             
GPB12    CLC   0(3,R2),WORK                                                     
         BE    GPB14                                                            
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GPB10               RETURN TO LOOP                               
GPB14    MVC   WORK(1),3(R2)      SET 1 CHAR PRD CODE                           
GPBX     B     EXIT                                                             
         SPACE                                                                  
*************************************                                           
* READS BN PROFILE / BILLING ON ASSIGNED                                        
*    OUTPUT: ASSIGNB=Y                                                          
*                                                                               
GETPRF   NTR1                                                                   
         MVI   ASSIGNB,0                                                        
         XC    KEY,KEY             GET PROFILE CHK BILLING OPTION               
         MVC   KEY(4),=C'SOBN'                                                  
         MVC   KEY+4(2),NBEFFAGY                                                
         MVI   KEY+6,C'N'                                                       
         GOTO1 NBCLUNPK,DMCB,NBACTCLI,KEY+7                                     
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),OFFCDE                                                 
         L     R3,DATAMGR                                                       
         GOTO1 GETPROF,DMCB,KEY,WORK,(R3)                                       
         CLI   WORK+3,C'Y'         IS IT BILLED ON ACTUAL                       
         BE    *+8                                                              
         MVI   ASSIGNB,C'Y'       NO/SET 'BILLING ON ASSIGNED' SWITCH           
         XIT1                                                                   
                                                                                
                                                                                
                                                                                
         EJECT                                                                  
*                                                                               
* DATAMGR WRITE TO DIRECTORY                                                    
WRTDIR   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',FILENAME,KEY,KEY                          
         J     EXIT                                                             
*                                                                               
* DATAMGR GETREC                                                                
IOGET    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         B     EXIT                                                             
*                                                                               
IOPUT    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         B     EXIT                                                             
*                                                                               
SPOOLIT  NTR1                                                                   
         CLI   DELETESW,C'S'       SUMMARY ONLY                                 
         BE    EXIT                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
*                                                                               
         GETEL R2,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
HDRTN    NTR1                                                                   
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H3(6),=C'CLIENT'                                                 
         MVC   H3+10(3),SPLCLI                                                  
         MVC   H3+15(20),SPLCLIN                                                
         OC    H3+15(20),SPACES                                                 
         MVC   H4(7),=C'PRODUCT'                                                
         MVC   H4+10(3),SPLPRO                                                  
         MVC   H4+15(20),SPLPRON                                                
         OC    H4+15(20),SPACES                                                 
         MVC   H5(8),=C'ESTIMATE'                                               
*                                                                               
***      LA    R3,SPLEST                                                        
         MVC   WORK(7),SPLEST      NEED TO GET 0S OUT OF HEADS                  
         OC    WORK(7),SPACES      FOR CDROM                                    
         LA    R3,WORK                                                          
***                                                                             
         MVC   H5+10(7),0(R3)                                                   
         LA    R5,4                                                             
         LA    R4,H5+10                                                         
ESTLOOP  CLI   0(R3),C','          IS IT RANGE                                  
         BNE   ESTLP5                                                           
         CLI   ESTHDOPT,1                                                       
         BE    HDRTN5                                                           
         B     ESTLP7                                                           
ESTLP5   MVC   0(1,R4),0(R3)                                                    
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         BCT   R5,ESTLOOP                                                       
         MVC   H5+22(20),SPLESTN                                                
         OC    H5+22(20),SPACES                                                 
*                                                                               
         CLI   ESTHDOPT,1                                                       
         BE    HDRTN5                                                           
         CLC   =C'ALL',H5+10            IF EST NOT = ALL                        
         BNE   HDRTN5                                                           
ESTLP7   XC    H5+10(27),H5+10                                                  
         OC    H5+10(27),SPACES                                                 
         LA    R3,H5+10                                                         
         ZIC   R1,ESTBSV                 THEN PUT EST IN HEADLINE               
         EDIT  (R1),(3,0(R3)),ALIGN=LEFT                                        
*                                                                               
HDRTN5   CLI   SWIT,0                                                           
         BE    HDHK1A                                                           
         CLI   SWIT,C'E'                                                        
         BNE   *+12                                                             
         BRAS  RE,ESTHEAD                                                       
         B     HDHK1A                                                           
         CLI   SWIT,C'B'                                                        
         BNE   *+12                                                             
         BAS   RE,BILLHEAD                                                      
         B     HDHK1A                                                           
         CLI   SWIT,C'S'                                                        
         BNE   *+12                                                             
         BAS   RE,STABHEAD                                                      
         B     HDHK1A                                                           
         CLI   SWIT,C'U'                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,UNTHEAD                                                       
*                                                                               
HDHK1A   DS    0H                                                               
*        CLI   BOXSET,C'Y'          SET PARAMS FOR BOXES                        
*        BE    HDX                                                              
*        MVI   BOXSET,C'Y'                                                      
*        L     R1,ABOX                                                          
*        USING BOXD,R1                                                          
*        LTR   R1,R1               IS ABOX ZEROS                                
*        BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
*        MVC   BOXCOLS,SPACES                                                   
*        MVC   BOXROWS,SPACES                                                   
*        MVI   BOXYORN,C'Y'                                                     
*        MVI   BOXWT,1                                                          
*        MVI   BOXINIT,0                                                        
*        MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
*        SPACE                                                                  
*        LA    R5,BOXCOLS                                                       
*        USING BPLINED,R5                                                       
*        SPACE                                                                  
**       DROP  R5                                                               
HDX      XIT1                                                                   
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
         SPACE 2                                                                
*************************************                                           
* MANUAL(STAB) HEAD                                                             
*                                                                               
STABHEAD NTR1                                                                   
         MVC   P3,P                                                             
**       XC    P,P                                                              
         BAS   RE,CLEARP                                                        
         MVC   P(22),=C'MANUAL BILLING RECORDS'                                 
         LA    R1,P2                                                            
         USING BPLINED,R1                                                       
         MVC   BPCLT(3),=C'CLT'                                                 
         MVC   BPPRD(3),=C'PRD'                                                 
         MVC   BPEST(3),=C'EST'                                                 
         MVC   BPINVO(4),=C'INVO'                                               
         MVC   BPDATE(4),=C'DATE'                                               
         MVC   BPAMT(3),=C'AMT'                                                 
         DROP  R1                                                               
         B     EXIT                                                             
         SPACE                                                                  
****************************************                                        
* HEADLINE FOR BILL RECORDS                                                     
*                                                                               
BILLHEAD NTR1                                                                   
         MVC   P4,P                                                             
***      XC    P,P                                                              
         BAS   RE,CLEARP                                                        
         MVC   P(15),=C'BILLING RECORDS'                                        
         LA    R2,P2                                                            
         USING BPLINED,R2                                                       
         MVC   BPCLT+132,=C'CLT'                                                
         MVC   BPPRD+132,=C'PRD'                                                
         MVC   BPEST+132,=C'EST'                                                
         MVC   BPMNSRV(5),=C'MN/YR'                                             
         MVC   BPMNSRV+132(5),=C'SERVC'                                         
*        MVC   BPMNYR(5),=C'MN/YR'                                              
*        MVC   BPMNYR+132(4),=C'BILL'                                           
*        MVC   BPNUM(4),=C'BILL'                                                
*        MVC   BPNUM+132(3),=C'NUM'                                             
         MVC   BPINVO+132(5),=C'INVNO'                                          
         MVC   BPDATE(4),=C'BILL'                                               
         MVC   BPDATE+132(4),=C'DATE'                                           
         MVC   BPAMT+5(4),=C'BILL'                                              
         MVC   BPAMT+138(3),=C'AMT'                                             
         B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
*************************************                                           
*                                                                               
CLEARP   DS    0H              CLEARING TO 0S MAKES CDROM GO BLANKS             
         MVI   P,X'40'         SO LETS TRY BLANKS                               
         MVC   P+1(L'P-1),P                                                     
         BR    RE                                                               
**                                                                              
         LTORG                                                                  
CLTTBL   DS    CL1200              3X400 CLIENTS                                
         DC    X'FF'                                                            
CLERRTBL DS    CL600               3X200 CLIENTS                                
         DC    X'FF'                                                            
****************************************                                        
* HEADLINE FOR UNIT PRINTOUT                                                    
*                                                                               
UNTHEAD  NTR1  BASE=*,LABEL=*                                                   
         MVC   P3,P                                                             
         MVI   P,X'40'         SO LETS TRY BLANKS                               
         MVC   P+1(L'P-1),P                                                     
****     BAS   RE,CLEARP                                                        
         MVC   P(12),=C'UNIT RECORDS'                                           
         LA    R2,P2                                                            
         USING UPLINED,R2                                                       
         MVC   UPCLT,=C'CLT'                                                    
         MVC   UPPRD,=C'PRD'                                                    
         MVC   UPPAK,=C'PKG'                                                    
         MVC   UPNET,=C'NTWK'                                                   
         MVC   UPPROG(4),=C'PROG'                                               
         MVC   UPDATE(4),=C'DATE'                                               
         MVC   UPACTUAL+1(7),=C'ORDERED'                                        
         MVC   UPINTEG+3(5),=C'INTEG'                                           
         MVC   UPASSGN(8),=C'ASSIGNED'                                          
         MVC   UPBILT-1(9),=C'BILL TIME'                                        
         MVC   UPBILI-1(9),=C'BILL INTG'                                        
         MVC   UPPAIDT-1(9),=C'PAID TIME'                                       
         MVC   UPPAIDI-1(9),=C'PAID INTG'                                       
         XIT1                                                                   
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
* HANDLE STAB NEW 3 CHARACTER PROD PASSIVES                                     
STABPASV NTR1  BASE=*,LABEL=*                                                   
         CLI   DELETESW,C'Y'                                                    
         BNE   STABPX                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E81'                                                  
         MVC   KEY+2(3),ESTKEYSV+1     AM/CLT                                   
         MVC   KEY+8(1),ESTKEYSV+7     ESTIMATE                                 
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
STABP5   CLC   KEY(5),KEYSAVE                                                   
         BNE   STABPX                                                           
         CLC   KEY+8(1),KEYSAVE+8                                               
         BNE   STABP7                                                           
         OI    KEY+13,X'C0'                                                     
         BAS   RE,WRTDIR                                                        
STABP7   GOTO1 SEQ                                                              
         B     STABP5                                                           
STABPX   XIT1                                                                   
         LTORG                                                                  
*                                                                               
                                                                                
* HEADLINE FOR ESTIMATE PRINTING                                                
*                                                                               
ESTHEAD  NTR1  BASE=*,LABEL=*                                                   
         MVC   P3,P                                                             
         MVI   P,X'40'         SO LETS TRY BLANKS                               
         MVC   P+1(L'P-1),P                                                     
         MVC   P(15),=C'ESTIMATE HEADER'                                        
         CLI   ASSIGNB,C'Y'                                                     
         BNE   *+10                                                             
         MVC   P+17(27),=C'*** BILLING ON ASSIGNED ***'                         
         LA    R2,P2                                                            
         USING BPLINED,R2                                                       
         MVC   BPCLT,=C'CLT'                                                    
         MVC   BPPRD,=C'PRD'                                                    
         MVC   BPEST,=C'EST'                                                    
         MVC   BPEST+7(4),=C'DATE'                                              
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
* COME HERE IF IN TEST MODE - ELSE KEEPS COUNT IN BILLRC                        
CNTUNTBL NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E0A'         ACTIVE X'04'                             
         L     R1,NBAIO                                                         
         MVC   KEY+2(19),1(R1)                                                  
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'XSPDIR  '                                            
         GOTO1 HIGH                                                             
CNTUNT5  CLC   KEY(21),KEYSAVE         IF NOT THERE SKIP                        
         BNE   CNTUNX                                                           
         L     R1,UNTBLCNT                                                      
         LA    R1,1(R1)                                                         
         ST    R1,UNTBLCNT                                                      
         GOTO1 SEQ                                                              
         B     CNTUNT5                                                          
CNTUNX   XC    FILENAME,FILENAME                                                
         XIT1                                                                   
*                                                                               
BILLRC   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E0A'         ACTIVE X'04'                             
         MVC   KEY+2(19),UNITKSV+1                                              
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'XSPDIR  '                                            
         GOTO1 HIGH                                                             
BILRC10  CLC   KEY(21),KEYSAVE         IF NOT THERE SKIP                        
         BNE   BILRCX                                                           
         OI    KEY+32,X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',FILENAME,KEY,KEY                          
*                                                                               
         MVC   FILENAME,=C'XSPFIL  '                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+36,NBAIO,(0,DMWORK)         
         L     R1,NBAIO                                                         
         OI    34(R1),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+36,NBAIO,(0,DMWORK)         
         L     R1,UNTBLCNT                                                      
         LA    R1,1(R1)                                                         
         ST    R1,UNTBLCNT                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E06'                                                  
         LA    R1,KEY                                                           
         USING NUBKEY,R1                                                        
         LA    R2,UNITKSV                                                       
         USING NUKPKEY,R2                                                       
         MVC   NUBKAM,NUKAM                                                     
         MVC   NUBKCLI,NUKCLT                                                   
         MVC   NUBKNET,NUKNET                                                   
         MVC   NUBKPROG,NUKPROG                                                 
         MVC   NUBKDATE,NUKDATE                                                 
         MVC   NUBKEST,NUKEST                                                   
         MVC   NUBKSUB,NUKSUB                                                   
         MVC   NUBKDPT,NUKDP                                                    
         MVC   FILENAME,=C'XSPDIR  '                                            
         DROP  R1,R2                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         OI    KEY+32,X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',FILENAME,KEY,KEY                          
*                               ANY MORE BILL RECS FOR THIS UNIT?               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E0A'         ACTIVE X'04'                             
         MVC   KEY+2(19),UNITKSV+1                                              
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'XSPDIR  '                                            
         GOTO1 HIGH              THIS REC IS DELETED SO WE SHOULD JUMP          
*                                OVER DELETED REC TO NXT VALID REC              
         B     BILRC10                                                          
*                                                                               
BILRCX   XC    FILENAME,FILENAME                                                
         XIT1                                                                   
*                                                                               
LRINIT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,BOOKVAL                                                       
         LA    R1,300(R1)          TAKEN BY DCB STORAGE                         
         USING AGYTOTD,R1                                                       
         MVC   ADISKSV,ADISKSVV                                                 
         DROP  R1                                                               
         L     R2,=F'200000'          SET MAX RECS FOR CLOSEOUT                 
         ST    R2,TBLMAXRC                                                      
         LA    R2,5                                                             
         ST    R2,TBLRCLEN                                                      
*                                                                               
         L     R2,TWADCONS                                                      
         USING TWADCOND,R2                                                      
         L     R2,TSPFUSER         IF TABLE IS ALREADY LOADED                   
         OC    0(4,R2),0(R2)                                                    
         BNZ   LOD3                DON'T LOAD IT AGAIN                          
         LOAD  EP=NETTAB,ERRET=LOADERR                                          
         B     *+6                 ELSE LOAD INVOICE TABLE                      
LOADERR  DC    H'0'                                                             
         ST    R0,0(R2)            SET A( TABLE) IN CORE RES AREA               
LOD3     MVC   ASUMRECD,0(R2)                                                   
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DAYLKUP  EQU   *                                                                
         DC    CL3'ALL',XL1'FF'                                                 
         DC    CL3'M-F',XL1'08'                                                 
         DC    CL3'MON',XL1'01'                                                 
         DC    CL3'TUE',XL1'02'                                                 
         DC    CL3'WED',XL1'03'                                                 
         DC    CL3'THU',XL1'04'                                                 
         DC    CL3'FRI',XL1'05'                                                 
         DC    CL3'SAT',XL1'06'                                                 
         DC    CL3'SUN',XL1'07'                                                 
         DC    CL3'M-S',XL1'09'                                                 
         DC    CL3'VAR',XL1'0A'                                                 
         DC    XL3'FFFFFF',CL1' '      END OF TABLE                             
         SPACE 2                                                                
*                                                                               
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C' NETWORK CLOSEOUT REPORT'                                
         SSPEC H2,52,C' -----------------------'                                
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
* READS CLIENT REC TO GET CLIENT PROFILE FOR CLUNPK                             
* SETS PRINTABLE CLIENT CODE INTO ACTVCLT                                       
RDCLT    NTR1  BASE=*,LABEL=*                                                   
         XC    KEY2,KEY2                                                        
         MVC   KEY2+1(3),WORK      AGY/CLIENT                                   
         MVC   WORK(13),KEY2                                                    
         LA    R1,=C'DMRDHI  '                                                  
         ST    R1,DMCB                                                          
         GOTO1 DATAMGR,DMCB,,=C'SPTDIR  ',KEY2,KEY2,0                           
         CLC   WORK(13),KEY2                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,=C'GETREC  '                                                  
         ST    R1,DMCB                                                          
         GOTO1 DATAMGR,DMCB,,=C'SPTFIL  ',KEY2+14,MYIO,(0,MYDMWK)               
         LA    R2,MYIO                                                          
         USING CLTHDR,R2                                                        
         L     R3,NBAIO                                                         
         GOTO1 NBCLUNPK,DMCB,(CPROF+6,2(R3)),ACTVCLT                            
         XIT1                                                                   
         LTORG                                                                  
KEY2     DS    CL27                                                             
         DROP  R2                                                               
                                                                                
MYIO     DS    CL2000                                                           
MYDMWK   DS    12D                                                              
*                                                                               
* ADD 'BAD' CLIENT TO CLERRTBL                                                  
*                                                                               
CLTERRTB NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,RDCLT       PUTS PRINTABLE CLIENT CODE IN ACTVCLT             
         L     R1,=A(CLERRTBL)                                                  
CLERR05  CLI   0(R1),X'FF'         EOF?                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(3,R1),ACTVCLT     ALREADY IN TABLE?                            
         BE    CLERRX              YES                                          
         CLI   0(R1),0                                                          
         BE    CLERR10                                                          
         LA    R1,3(R1)                                                         
         B     CLERR05                                                          
CLERR10  MVC   0(3,R1),ACTVCLT                                                  
CLERRX   XIT1                                                                   
         LTORG                                                                  
                                                                                
           EJECT                                                                
                                                                                
* WRITING TRAFFIC AND INVOICE CLOSEOUT REQUESTS                                 
*                                                                               
ADDREQS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,CLTTBL           CLIENT TABLE                                 
ADD05    L     R5,=A(CLERRTBL)         ERROR CLIENTS                            
         CLI   0(R4),0             END OF CLIENT                                
         BE    ADD30               THAT'S ALL                                   
*                                                                               
ADD07    CLC   0(3,R4),0(R5)       IF CLIENT = ERROR CLIENT                     
         BE    ADD10               SKIP THIS CLIENT                             
*                                                                               
         LA    R5,3(R5)            BUMP ERROR CLIENT TABLE                      
         CLI   0(R5),0             END OF ERROR TABLE                           
         BE    ADD20               YES-ADD CLIENT TO REQS                       
         BNE   ADD07               NO-CHK AGAINST CLIENT                        
*                                                                               
ADD10    LA    R4,3(R4)            BUMP CLIENT TABLE                            
         B     ADD05               START AT ERROR CLIENTS                       
                                                                                
ADD20    MVC   I8REQST,SPACES                                                   
         USING QRECORD,R3                                                       
         LA    R3,I8REQST                                                       
         MVC   QCODE,=C'I8'                                                     
         MVC   QAGY,NBSELAGY     AGENCY                                         
         MVC   QMED,NBSELMED     MEDIA                                          
         MVC   QCLT,0(R4)        CLIENT                                         
         MVC   QSTART(4),=C'8601' START DATE                                    
         MVC   QEND,REQSTEND    END DATE                                        
         MVI   QOPT2,C'P'        PRINT ALL DETAILS                              
         MVC   QOPT4,PUFSW       SET PU OPTION                                  
         MVI   QOPT5,C'Y'        TEST=Y IS DEFAULT                              
         CLI   DELETESW,C'Y'     DELETE RECORDS?                                
         BNE   *+8                                                              
         MVI   QOPT5,C'N'        DELETE RECS                                    
         DROP  R3                                                               
*                                                                               
***      L     R3,AI8RQDCB                                                      
***      LA    R0,I8REQST                                                       
***      PUT   (R3),(R0)                                                        
         L     R3,BOOKVAL          IQRQFIL                                      
         LA    R0,I8REQST                                                       
         PUT   (R3),(R0)                                                        
***      PUT   I8RQFIL,(R0)                                                     
*                                                                               
         MVC   I8REQST(80),SPACES                                               
         MVC   I8REQST(2),=C'PR'                                                
         MVC   I8REQST+2(2),NBSELAGY                                            
         MVC   I8REQST+4(26),=CL26'*.PURGE.PURGE..DDS,T/A....'                  
         MVC   I8REQST+30(1),NBSELMED                                           
         MVI   I8REQST+31,C'.'                                                  
         MVC   I8REQST+32(3),0(R4)                                              
         MVC   I8REQST+35(3),=C'...'                                            
         GOTO1 DATCON,DMCB,(0,REQSTEND),(5,I8REQST+38)                          
         LA    R1,I8REQST+46                                                    
         MVI   0(R1),C'.'                                                       
         AHI   R1,1                                                             
*                                                                               
         CLI   PUFSW,C'Y'                                                       
         BNE   *+14                                                             
         MVC   0(7,R1),=C'SKIPPU,'                                              
         AHI   R1,7                                                             
*                                                                               
         CLI   DELETESW,C'Y'       DELETE RECS?                                 
         BE    *+14                                                             
         MVC   0(5,R1),=C'TEST.'   NO                                           
         AHI   R1,5                                                             
*                                                                               
         BCTR  R1,0                LAST CHAR WILL BE . OR ,                     
         MVC   0(2,R1),=C'.*'      SO FORCE TO .                                
*                                                                               
***      L     R3,ATRRQDCB                                                      
***      LA    R0,I8REQST                                                       
***      PUT   (R3),(R0)                                                        
*                                                                               
         L     R3,BOOKVAL                                                       
         LA    R3,150(R3)          BUMP TO TRRQFIL                              
         LA    R0,I8REQST                                                       
         PUT   (R3),(R0)                                                        
***      PUT   TRRQFIL,(R0)                                                     
*                                                                               
         B     ADD10               GO GET NEXT CLIENT                           
*                                                                               
ADD30    DS    0H                  CLEAR TABLES                                 
         LA    RE,CLTTBL                                                        
         LA    RF,L'CLTTBL                                                      
         XCEF                                                                   
         L     RE,=A(CLERRTBL)                                                  
         LA    RF,L'CLERRTBL                                                    
         XCEF                                                                   
*                                                                               
         XIT1                                                                   
*                                                                               
OPENRQS  NTR1  BASE=*,LABEL=*                                                   
         L     R1,BOOKVAL          STORE DCB IN BOOKVAL                         
         LA    RE,I8RQFIL                                                       
         MVC   0(128,R1),0(RE)     I8RQFIL                                      
         LA    R1,150(R1)                                                       
         LA    RE,TRRQFIL                                                       
         MVC   0(128,R1),0(RE)     TRRQFIL                                      
                                                                                
* OPEN FILES                                                                    
         L     R2,BOOKVAL          I8RQFIL                                      
         OPEN  ((R2),(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,BOOKVAL                                                       
         LA    R2,150(R2)          BUMP TO TRRQFIL                              
         OPEN  ((R2),(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
*                                                                               
CLOSRQS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,BOOKVAL                                                       
         CLOSE ((R2))               I8RQFIL                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,BOOKVAL                                                       
         LA    R2,150(R2)          BUMP TO TRRQFIL                              
         CLOSE ((R2))                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                                                                   
*                                                                               
         DS    0D                                                               
I8RQLBL  DC    CL8'*I8RQFL*'                                                    
I8RQFIL  DCB   DDNAME=I8RQFIL,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=80,MACRF=PM                                                
*                                                                               
         DS    0D                                                               
TRRQLBL  DC    CL8'*TRRQFL*'                                                    
TRRQFIL  DCB   DDNAME=TRRQFIL,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=80,MACRF=PM                                                
*                                                                               
         LTORG                                                                  
*                                                                               
I8REQST   DS    CL(L'QAREA)                                                     
*                                                                               
*                                 CL4=DISK AD, CL1=WARNING MARK                 
         EJECT                                                                  
********************************************                                    
* RUNFIRST CLEARS TOTALS FOR RUNLAST                                            
* IN CASE OF MULTIPLE REQUESTS FOR SAME AGY                                     
FIRSTRUN DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**FRUN**,RR=R2                                                 
         L     RC,0(R1)                                                         
         L     R2,BOOKVAL                                                       
         LA    R2,300(R2)          FOR DCB STORAGE                              
         USING AGYTOTD,R2                                                       
         MVI   MULTREQ,0                                                        
*        LA    R2,AGYTOTS                                                       
         LA    R3,11                                                            
FZLOOP   ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,FZLOOP                                                        
         L     RE,ATWA                                                          
         MVI   29(RE),2           SET TWAFIRST FOR RUNLAST HOOK                 
*                                                                               
                                                                                
*        GET STORAGE FOR DISK ADDRESSES                                         
         L     R1,=F'200000'          SET MAX RECS FOR CLOSEOUT                 
         ST    R1,TBLMAXRC                                                      
         LA    R1,5                                                             
         ST    R1,TBLRCLEN                                                      
*                                                                               
         L     R3,TBLMAXRC                                                      
         L     RE,TBLRCLEN                                                      
         MR    R2,RE                                                            
         AHI   R3,4                EXTRA 4 BYTES FOR TABLE LENGTH               
         ST    R3,DMCB+4           MIN/MAX REQUESTED                            
         ST    R3,DMCB+8                                                        
         ST    R3,DSKSVLEN         SAVE TABLE LENGTH TO FREE LATER              
         GOTO1 =V(COVAIL),DMCB,C'GET'                                           
         ICM   RE,15,4(R1)         ADDRESS OF SAVE AREA                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R3,0(RE)            +0 = L'TABLE                                 
         AHI   R3,-4                                                            
         AHI   RE,4                                                             
         L     R2,BOOKVAL                                                       
         LA    R2,300(R2)          FOR DCB STORAGE                              
         ST    RE,ADISKSVV         ADDRESS OF DISK SAVE AREA                    
         XCEF  (RE),(R3)                                                        
*                                                                               
         BRAS RE,OPENRQS          OPEN I8/TR REQUEST FILES                      
**********************************                                              
                                                                                
FIRSTX   XIT1                                                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
********************************************                                    
* LASTRUN PRINTS TOTALS ACROSS AGENCY REQUESTS                                  
* IN CASE OF MULTIPLE REQUESTS FOR SAME AGY                                     
LASTRUN  DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**LRUN**,RR=R2                                                 
         L     RC,0(R1)                                                         
         L     R2,BOOKVAL                                                       
         LA    R2,300(R2)          FOR DCB STORAGE                              
         USING AGYTOTD,R2                                                       
         MVI   P,X'40'                                                          
         MVC   P+1(L'P-1),P                                                     
         MVC   P+38(7),=C'ORDERED'                                              
         MVC   P+54(8),=C'ASSIGNED'                                             
         MVC   P+73(6),=C'BILLED'                                               
         MVC   P+92(4),=C'PAID'                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,P                                                             
         MVC   P(25),=C'*** ALL AGENCY TOTALS ***'                              
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),0(R2)                                                   
         MVC   32(15,R3),WORK+3                                                 
                                                                                
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),8(R2)                                                   
         MVC   49(15,R3),WORK+3                                                 
                                                                                
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),24(R2)                                                  
         MVC   83(15,R3),WORK+3                                                 
                                                                                
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),16(R2)                                                  
         MVC   66(15,R3),WORK+3                                                 
*****                                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(12),=C'ESTIMATES = '                                           
         EDIT  (P8,AGYEST),(10,P+12),ALIGN=LEFT                                 
         MVC   P2(8),=C'PACKAGES'                                               
         MVI   P2+10,C'='                                                       
         EDIT  (P8,AGYPKG),(10,P2+12),ALIGN=LEFT                                
         MVC   P3(5),=C'UNITS'                                                  
         MVI   P3+10,C'='                                                       
         EDIT  (P8,AGYUNT),(10,P3+12),ALIGN=LEFT                                
         MVC   P4(5),=C'BILLS'                                                  
         MVI   P4+10,C'='                                                       
         EDIT  (P8,AGYBILL),(10,P4+12),ALIGN=LEFT                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(7),=C'MANUALS'                                                 
         MVI   P+10,C'='                                                        
         EDIT  (P8,AGYSTAB),(10,P+12),ALIGN=LEFT                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(7),=C'HISTORY'                                                 
         MVI   P+10,C'='                                                        
         EDIT  (B4,HISTCNT),(10,P+12),ALIGN=LEFT                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BAS   RE,PUCLNTS            CLIENTS SKIPPED                            
         L     R2,ADISKSV            R2-> START OF TABLE                        
         AHI   R2,-4                                                            
         L     R3,0(R2)              R3->TBL LENGTH                             
         GOTO1 =V(COVAIL),DMCB,C'FREE',(R2),(R3)                                
*                                                                               
         BRAS  RE,CLOSRQS                                                       
*                                                                               
LASTX    XIT1                                                                   
*                                                                               
         DROP  R2                                                               
*                                                                               
PUCLNTS  NTR1                                                                   
         L     R2,=A(SKIPCLTB)                                                  
         OC    0(3,R2),0(R2)                                                    
         BZ    PUCXX                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(35),=C'*** PU PROFILE EXCLUDED CLIENTS ***'                    
PUC4     LA    R3,10                                                            
         LA    R1,P2                                                            
PUC5     MVC   0(3,R1),0(R2)                                                    
         LA    R1,4(R1)                                                         
         LA    R2,3(R2)                                                         
         OC    0(3,R2),0(R2)                                                    
         BZ    PUCX                                                             
         BCT   R3,PUC5                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PUC4                                                             
PUCX     GOTO1 SPOOL,DMCB,(R8)                                                  
PUCXX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*************************************                                           
* READS PU PROFILE                                                              
* WORK+2 = Y MEANS SKIP CLIENT FOR CLOSE OUT                                    
*                                                                               
*                                                                               
CHKPU    NMOD1 0,**CKPU**,RR=R2                                                 
         L     RC,0(R1)                                                         
*                                                                               
         BAS   RE,CHKCLTRC         HAVE WE READ CLIENT RECORD?                  
         XC    WORK(10),WORK                                                    
         XC    KEY,KEY             GET PROFILE CHK BILLING OPTION               
         MVC   KEY(4),=C'SOPU'                                                  
         MVC   KEY+4(2),NBEFFAGY                                                
         MVI   KEY+6,C'N'                                                       
         GOTO1 NBCLUNPK,DMCB,NBACTCLI,KEY+7                                     
         MVI   KEY+10,C'*'                                                      
         MVC   KEY+11(1),OFFCDE                                                 
         L     R3,DATAMGR                                                       
         GOTO1 GETPROF,DMCB,KEY,WORK,(R3)                                       
         CLI   PUFSW,C'O'           ONLY PROCESS PU CLIENTS                     
         BNE   CHKPU05                                                          
         CLI   WORK+3,C'Y'         SAVE CLIENTS TO PROCESS                      
         BE    CHKPU10             IN SKIPCLTB                                  
*                                                                               
CHKPU05  CLI   WORK+3,C'Y'         SKIP THIS CLIENT?                            
         BNE   CHKPUX                                                           
*                                  YES/SAVE SKIPPED CLT IN TBL                  
         L     R1,=A(SKIPCLTB)     ADDRESS OF SKIPPED CLT TABLE                 
CHKPU10  CLI   0(R1),0                                                          
         BE    CHKPU12                                                          
         LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   CHKPU10                                                          
         DC    H'0'                TABLE LIMIT                                  
CHKPU12  MVC   0(3,R1),KEY+7                                                    
         CLI   WORK+3,C'Y'         RESET CC                                     
CHKPUX   XIT1                      BE -> SKIP  BNE -> DONT SKIP                 
         LTORG                                                                  
SKIPCLTB DS    CL3000              ROOM FOR 1000 CLIENTS                        
         DC    X'FF'                                                            
**********************************************************                      
                                                                                
* IT'S POSSIBLE TO GET HERE WITHOUT HAVING READ CLIENT REC                      
* AND THUS NOT HAVE CLIST OR OFFICE CODE.  (IN OFFICE FILTER                    
* REQUEST, FOR EXAMPLE).                                                        
* DUE TO FACT THAT OVER YEARS NETIO MODE SEQUENCES HAVE BECOME XXX              
* IF NO CLIST, ROUTINE GETS CLIST AND SETS UP OFFICE CODE                       
CHKCLTRC NTR1                                                                   
         L     R1,ANETWS4          DO WE HAVE CLIST?                            
         LA    R1,2000(R1)                                                      
         OC    0(3,R1),0(R1)                                                    
         BNZ   CHKCLX                                                           
         GOTO1 AGETCLST,DMCB,(RC)  GETS CLIST                                   
         MVC   OFFCDE,BYTE         OFFICE CODE PASSED IN BYTE                   
CHKCLX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
*********************************************************                       
         EJECT                                                                  
********************************************                                    
* SUMMARY ON LAST PAGE OF REPORT                                                
*                                                                               
SUMMARY  DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**SUMY**,RR=R2                                                 
         L     RC,0(R1)                                                         
*                                                                               
         MVI   ESTHDOPT,1                                                       
         L     R3,ASUMRECD                                                      
         L     R4,SUMCNTR                                                       
         LTR   R4,R4                                                            
         BZ    SUMX                                                             
***      XC    P,P              NEED TO CLEAR P LINES SINCE IN                  
         MVI   P,X'40'                                                          
         MVC   P+1(L'P-1),P                                                     
***      XC    P2,P2            -SUMMARY ONLY OPTION- P LINES ARE               
***      XC    P3,P3            FILLED BUT NOT PRINTED(SO NOT CLEARED)          
***      XC    P4,P4                                                            
         MVC   P2,P                                                             
         MVC   P3,P2                                                            
         MVC   P4,P3                                                            
*                                                                               
         MVI   SPACING,2              SKIP LINES                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(25),=C'RECAP OF CLOSED ESTIMATES'                              
         MVI   P+132,C'-'                                                       
         MVC   P+133(24),P+132                                                  
*        MVC   P4(17),=C'ESTIMATES CLOSED='                                     
*        EDIT  (B4,SUMCNTR),(6,P4+18),ALIGN=LEFT                                
         L     R2,SUMCNTR                                                       
         CVD   R2,DUB                                                           
         AP    TOTEST,DUB                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+38(7),=C'ORDERED'                                              
         MVC   P+54(8),=C'ASSIGNED'                                             
         MVC   P+73(6),=C'BILLED'                                               
         MVC   P+92(4),=C'PAID'                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
SUM10    MVC   P(100),0(R3)                                                     
         CLI   P+29,X'40'                                                       
         BE    SUM11                                                            
         MVC   P+100(24),=C'*** WARNING - ERRORS ***'                           
         CLI   P+29,1                                                           
         BE    SUM11                                                            
         MVC   P+100(24),=C'*** NET/BILL ERROR ***'                             
SUM11    XC    P+4(4),P+4          DELETE PRD CODE                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
SUM12    LA    R3,100(R3)                                                       
         BCT   R4,SUM10                                                         
         XC    SUMCNTR,SUMCNTR                                                  
*                                                                               
***      XC    P,P                                                              
         MVI   P,X'40'                                                          
         MVC   P+1(L'P-1),P                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,P                                                             
         MVC   P(14),=C'*** TOTALS ***'                                         
         LA    R2,RUNTOTS                                                       
*****    EDIT  (P8,0(R2)),(15,32(R3)),2,MINUS=YES UNTORD2                       
**       EDIT  (P8,0(R2)),(15,32(R3)),2,MINUS=YES UNTORD2                       
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),0(R2)                                                   
         MVC   32(15,R3),WORK+3                                                 
*                                                                               
**       EDIT  (P8,8(R2)),(15,49(R3)),2,MINUS=YES ASSIGNED-DROP CENTS           
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),8(R2)                                                   
         MVC   49(15,R3),WORK+3                                                 
*                                                                               
**       EDIT  (P8,24(R2)),(15,83(R3)),2,MINUS=YES UNTPAID2                     
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),24(R2)                                                  
         MVC   83(15,R3),WORK+3                                                 
*                                                                               
**       EDIT  (P8,16(R2)),(15,66(R3)),2,MINUS=YES UNTBILL2                     
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),16(R2)                                                  
         MVC   66(15,R3),WORK+3                                                 
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(12),=C'ESTIMATES = '                                           
         EDIT  (P8,TOTEST),(10,P+12),ALIGN=LEFT                                 
         MVC   P2(8),=C'PACKAGES'                                               
         MVI   P2+10,C'='                                                       
         EDIT  (P8,TOTPKG),(10,P2+12),ALIGN=LEFT                                
         MVC   P3(5),=C'UNITS'                                                  
         MVI   P3+10,C'='                                                       
         EDIT  (P8,TOTUNT),(10,P3+12),ALIGN=LEFT                                
         MVC   P4(5),=C'BILLS'                                                  
         MVI   P4+10,C'='                                                       
         EDIT  (P8,TOTBILL),(10,P4+12),ALIGN=LEFT                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(7),=C'MANUALS'                                                 
         MVI   P+10,C'='                                                        
         EDIT  (P8,TOTSTAB),(10,P+12),ALIGN=LEFT                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(10),=C'NET BILLED'                                             
         MVI   P+11,C'='                                                        
         EDIT  (P8,TOTNETB),(13,P+12),2,ALIGN=LEFT                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(8),=C'NET PAID'                                                
         MVI   P+10,C'='                                                        
         EDIT  (P8,TOTNETP),(13,P+12),2,ALIGN=LEFT                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(14),=C'UNIT BILL RECS'                                         
         MVI   P+15,C'='                                                        
         EDIT  (B4,UNTBLCNT),(10,P+16),ALIGN=LEFT                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XC    UNTBLCNT,UNTBLCNT                                                
*                                                                               
         LA    R2,RUNTOTS                                                       
         LA    R3,FNLTOTS                                                       
         LA    R4,9                                                             
SUMFNL   AP    0(8,R3),0(8,R2)                                                  
         LA    R3,8(R3)                                                         
         LA    R2,8(R2)                                                         
         BCT   R4,SUMFNL                                                        
         AP    FNLNETP,TOTNETP                                                  
         AP    FNLNETB,TOTNETB                                                  
*                                                                               
         LA    R2,RUNTOTS                                                       
         LA    R3,9                                                             
SUMZAP   ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,SUMZAP                                                        
         ZAP   TOTNETB,=P'0'                                                    
         ZAP   TOTNETP,=P'0'                                                    
*                                                                               
         CLI   CALLDDS,C'Y'                                                     
         BNE   SUMX                                                             
         MVI   P,0                                                              
         MVI   P3,0                                                             
         MVC   P4(23),=C'   ***** CALL DDS *****'                               
         MVC   P2(36),=C'***** ERROR-NO ESTIMATE HEADER *****'                  
         MVI   CALLDDS,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
SUMX     L     R1,=A(SKIPCLTB)     ADDRESS OF SKIPPED CLT TABLE                 
         CLI   0(R1),0             ANY SKIPPED?                                 
         BE    SUMXX                                                            
SUMXX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
********************************************                                    
* RUNFINAL ON LAST PAGE OF REPORT                                               
*                                                                               
RUNFINAL DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**RUNF**,RR=R2                                                 
         L     RC,0(R1)                                                         
*                                                                               
         MVI   ESTHDOPT,1                                                       
**       XC    P,P                                                              
         MVI   P,X'40'                                                          
         MVC   P+1(L'P-1),P                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(18),=C'*** RUN TOTALS ***'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+38(7),=C'ORDERED'                                              
         MVC   P+54(8),=C'ASSIGNED'                                             
         MVC   P+73(6),=C'BILLED'                                               
         MVC   P+92(4),=C'PAID'                                                 
*                                                                               
         LA    R2,FNLTOTS                                                       
         LA    R3,P2                                                            
**       EDIT  (P8,0(R2)),(15,32(R3)),2,MINUS=YES UNTORD2                       
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),0(R2)                                                   
         MVC   32(15,R3),WORK+3                                                 
                                                                                
*        EDIT  (P8,8(R2)),(15,49(R3)),2,MINUS=YES ASSIGNED-DROP CENTS           
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),8(R2)                                                   
         MVC   49(15,R3),WORK+3                                                 
                                                                                
*        EDIT  (P8,24(R2)),(15,83(R3)),2,MINUS=YES UNTPAID2                     
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),24(R2)                                                  
         MVC   83(15,R3),WORK+3                                                 
                                                                                
*        EDIT  (P8,16(R2)),(15,66(R3)),2,MINUS=YES UNTBILL2                     
         MVC   WORK(18),=X'40202020202020202020202020214B202060'                
         ED    WORK(18),16(R2)                                                  
         MVC   66(15,R3),WORK+3                                                 
                                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P(12),=C'ESTIMATES = '                                           
         EDIT  (P8,FNLEST),(10,P+12),ALIGN=LEFT                                 
         MVC   P2(8),=C'PACKAGES'                                               
         MVI   P2+10,C'='                                                       
         EDIT  (P8,FNLPKG),(10,P2+12),ALIGN=LEFT                                
         MVC   P3(5),=C'UNITS'                                                  
         MVI   P3+10,C'='                                                       
         EDIT  (P8,FNLUNT),(10,P3+12),ALIGN=LEFT                                
         MVC   P4(5),=C'BILLS'                                                  
         MVI   P4+10,C'='                                                       
         EDIT  (P8,FNLBILL),(10,P4+12),ALIGN=LEFT                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(7),=C'MANUALS'                                                 
         MVI   P+10,C'='                                                        
         EDIT  (P8,FNLSTAB),(10,P+12),ALIGN=LEFT                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(10),=C'NET BILLED'                                             
         MVI   P+11,C'='                                                        
         EDIT  (P8,FNLNETB),(13,P+12),2,ALIGN=LEFT                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(8),=C'NET PAID'                                                
         MVI   P+10,C'='                                                        
         EDIT  (P8,FNLNETP),(13,P+12),2,ALIGN=LEFT                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R2,FNLTOTS                                                       
*        LA    R3,AGYTOTS                                                       
         L     R3,BOOKVAL                                                       
         LA    R3,300(R3)          FOR DCB STORAGE                              
         USING AGYTOTD,R3                                                       
         ZIC   R1,MULTREQ                                                       
         LA    R1,1(R1)                                                         
         STC   R1,MULTREQ                                                       
         LA    R4,9                                                             
AGYTOT   AP    0(8,R3),0(8,R2)                                                  
         LA    R3,8(R3)                                                         
         LA    R2,8(R2)                                                         
         BCT   R4,AGYTOT                                                        
         AP    0(8,R3),FNLNETB                                                  
         AP    8(8,R3),FNLNETP                                                  
*                                                                               
         LA    R2,FNLTOTS                                                       
         LA    R3,9                                                             
FNLZAP   ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,FNLZAP                                                        
         ZAP   FNLNETB,=P'0'                                                    
         ZAP   FNLNETP,=P'0'                                                    
RUNX     XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
************************************************                                
* ERROR HANDLING ROUTINES                                                       
*                                                                               
*                                                                               
ERRORS   DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**ERRS**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
*                                                                               
         CLI   KILLSW,C'Y'            IGNORE ERROR MESSAGE                      
         BNE   ER1                                                              
         MVI   ESTWARN,1           YES/SET ESTIMATE WARNING SWITCH              
         CLI   ERROR,3                                                          
         BNE   *+8                                                              
         MVI   ESTWARN,3                                                        
         MVC   UNTWARN,ERROR                                                    
         CLI   UNTWARN,3           FUDGE FOR NETBILL/PAY                        
         BNE   *+8                                                              
         MVI   UNTWARN,0                                                        
         CLI   UNTWARN,0           IF ZERO,CHANGE FOR BILLING ERROR             
         BNE   *+8                 WARNING UNIT                                 
         MVI   UNTWARN,4                                                        
         B     ERRXIT                  AND EXIT                                 
*                                                                               
ER1      ZIC   R1,ERROR               NO/PROCESS ERROR MESSAGE                  
         MH    R1,=H'35'                                                        
         LA    R2,ERRMSG                                                        
         AR    R2,R1                                                            
         MVC   P(13),=C'*** ERROR ***'                                          
         MVC   P+15(35),0(R2)         SET ERROR MESSAGE                         
         CLI   ERROR,3             IF NETPAID/BILLED ERROR                      
         BE    ERR3                                                             
         LA    R2,ERRPROC                                                       
ER5      CLC   ERROR,0(R2)         GET ERROPR PROC                              
         BE    ER7                                                              
         LA    R2,5(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   ER5                                                              
         DC    H'0'                                                             
ER7      MVC   FULL,1(R2)                                                       
         L     RF,FULL                                                          
         BR    RF                                                               
*                                                                               
ERR0     DS    0H                                                               
         MVC   P2+35(19),=C'GROSS BILLED+INTEG='                                
         L     R1,NBBILTGR         GET GROSS BILLED                             
         A     R1,NBBILIGR         ADD INTEGRATION                              
         LA    R3,P2+55                                                         
         EDIT  (R1),(13,0(R3)),2,ALIGN=LEFT                                     
         MVC   P2+70(17),=C'GROSS PAID+INTEG='                                  
         L     R1,NBPAYTGR         GROSS PAID                                   
         A     R1,NBPAYIGR         ADD INTEG                                    
         LA    R3,P2+88                                                         
         EDIT  (R1),(13,0(R3)),2,ALIGN=LEFT                                     
         B     ERRX                                                             
ERR3     DS    0H                                                               
         MVC   P2+35(13),=C'NET BILLED = '                                      
         LA    R3,P2+49                                                         
         ICM   R1,15,WORK+20                                                    
         EDIT  (R1),(13,0(R3)),2,ALIGN=LEFT                                     
         MVC   P2+70(11),=C'NET PAID = '                                        
         LA    R3,P2+82                                                         
         ICM   R1,15,WORK+24                                                    
         EDIT  (R1),(13,0(R3)),2,ALIGN=LEFT                                     
         B     ERRX                                                             
*                                                                               
ERR4     DS    0H                                                               
         MVC   DUB,WORK            WORK=BILLS+MANU TOTS                         
         MVC   P2+35(19),=C'UNIT BILLED TOTAL ='                                
         LA    R2,UNTBILL                                                       
         LA    R3,P2+55                                                         
         EDIT  (P8,0(R2)),(13,0(R3)),2,ALIGN=LEFT                               
         MVC   P2+70(17),=C'BILLED + MAN TOT='                                  
         LA    R3,P2+88                                                         
         EDIT  (P8,DUB),(13,0(R3)),2,ALIGN=LEFT                                 
*                                                                               
ERRX     DS    0H                                                               
         MVC   P2(3),NBCLICOD            CLIENT                                 
         LA    R2,P2+4                                                          
         EDIT  (B1,NBACTEST),(4,0(R2))    ESTIMATE                              
         MVC   P2+9(4),NBACTNET           NETWORK                               
         MVC   P2+14(1),NBACTDP           DAYPART                               
         MVC   P2+15(6),NBACTPRG          PROGRAM                               
         CLI   ERROR,4                                                          
         BE    ERRX2                                                            
         LA    R2,P2+22                                                         
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(8,0(R2))                               
         MVI   P2+30,C'-'                                                       
         EDIT  (B1,NBACTSUB),(3,P2+31),ALIGN=LEFT                               
*                                                                               
ERRX2    MVI   NBFUNCT,4              SET NETIO TO GET NEXT EST                 
         MVI   ERRORSW,1              SET ERROR SWITCH                          
         MVC   WORK(3),ESTKEYSV+1                                               
         BRAS  RE,CLTERRTB         ADD TO CLIENT ERROR TABLE                    
         MVI   FORCEHED,C'Y'                                                    
ERRXX    DS    0H                                                               
         CLI   DELETESW,C'S'                                                    
         BE    ERRXIT                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
ERRXIT   XIT1                                                                   
         EJECT                                                                  
*********************************                                               
*                                                                               
ERRPROC  DS    0H                                                               
         DC    AL1(0),AL4(ERR0)                                                 
         DC    AL1(1),AL4(ERR0)           SAME AS 0                             
         DC    AL1(2),AL4(ERR0)           SAME AS 0                             
         DC    X'00'                                                            
         SPACE 2                                                                
****************************************                                        
* ERROR MESSAGES                                                                
*                                                                               
ERRMSG   DS    0H                                                               
ERROR0   DC    CL35' UNIT BILLED/PAID NOT EQUAL'                                
ERROR1   DC    CL35' UNIT CURRENT MONTH'                                        
ERROR2   DC    CL35' ORDERED/PAID/BILLED NOT EQUAL'                             
ERROR3   DC    CL35' NET PAID/NET BILLED NOT EQUAL'                             
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************                                   
* PRINTS OUT AND CLEARS VARIOUS TOTAL FIELDS                                    
* ALSO CHKS IF 1ST READ TOTS=2ND READ TOTS                                      
*                                                                               
BREAKTOT DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**BRKT**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
*                                                                               
         MVC   P(19),=C'ESTIMATE HEADERS  ='                                    
         L     R2,ESTCNTR                                                       
         EDIT  (R2),(3,P+20),ALIGN=LEFT                                         
         BAS   RE,WRITEIT                                                       
*                                                                               
         MVC   P(9),=C'PACKAGES '                                               
         MVI   P+18,C'='                                                        
         LA    R2,P+20                                                          
         L     R3,PKGCNTR                                                       
         EDIT  (R3),(6,0(R2)),ALIGN=LEFT                                        
         CVD   R3,DUB                                                           
         AP    TOTPKG,DUB                                                       
         BAS   RE,WRITEIT                                                       
*                                                                               
         MVC   P(6),=C'UNITS '                                                  
         MVI   P+18,C'='                                                        
         LA    R2,P+20                                                          
         LA    R3,UNTCNTR                                                       
         CLC   0(4,R3),4(R3)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (B4,0(R3)),(6,0(R2)),ALIGN=LEFT                                  
         BAS   RE,WRITEIT                                                       
*                                                                               
         MVC   P(6),=C'BILLS '                                                  
         MVI   P+18,C'='                                                        
         L     R2,BILLCNTR                                                      
         EDIT  (R2),(4,P+20),ALIGN=LEFT                                         
         CLC   BILLCNTR,BILLCNT2                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         CVD   R2,DUB                                                           
         AP    TOTBILL,DUB                                                      
         BAS   RE,WRITEIT                                                       
*                                                                               
         MVC   P(7),=C'MANUAL '                                                 
         L     R2,STABCNTR                                                      
         MVI   P+18,C'='                                                        
         EDIT  (R2),(4,P+20),ALIGN=LEFT                                         
         CLC   STABCNTR,STABCNT2                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,WRITEIT                                                       
         CVD   R2,DUB                                                           
         AP    TOTSTAB,DUB                                                      
*                                                                               
         L     R3,UNTCNTR                                                       
         CVD   R3,DUB                                                           
         AP    TOTUNT,DUB                                                       
         MVC   P(12),=C'UNIT ORDERED'                                           
         LA    R2,P                                                             
         LA    R3,UNTORD                                                        
         CP    0(8,R3),8(8,R3)     TEST IF READ/DELETE NUMBERS ARE =            
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P8,0(R3)),(15,15(R2)),2,MINUS=YES                               
         BAS   RE,WRITEIT                                                       
         MVC   P(12),=C'UNIT ASSIGND'                                           
         LA    R2,P                                                             
         LA    R3,UNTASSGN                                                      
         CP    0(8,R3),8(8,R3)     TEST IF READ/DELETE NUMBERS ARE =            
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P8,0(R3)),(15,15(R2)),2,MINUS=YES                               
         BAS   RE,WRITEIT                                                       
         MVC   P(11),=C'UNIT BILLED'                                            
         LA    R2,P                                                             
         LA    R3,UNTBILL                                                       
         CP    0(8,R3),8(8,R3)     TEST IF READ/DELETE NUMBERS ARE =            
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P8,0(R3)),(15,15(R2)),2,MINUS=YES                               
         BAS   RE,WRITEIT                                                       
         MVC   P(9),=C'UNIT PAID'                                               
         LA    R2,P                                                             
         LA    R3,UNTPAID                                                       
         CP    0(8,R3),8(8,R3)     TEST IF READ/DELETE NUMBERS ARE =            
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P8,0(R3)),(15,15(R2)),2,MINUS=YES                               
         BAS   RE,WRITEIT                                                       
*                                                                               
         MVC   P(11),=C'BILLED RECS'                                            
         LA    R2,P                                                             
         LA    R3,BLRECAMT                                                      
         CP    0(8,R3),8(8,R3)     TEST IF READ/DELETE NUMBERS ARE =            
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P8,0(R3)),(15,15(R2)),2,MINUS=YES                               
         BAS   RE,WRITEIT                                                       
         MVC   P(13),=C'MANUAL BILLED'                                          
         LA    R2,P                                                             
         LA    R3,STABAMT                                                       
         CP    0(8,R3),8(8,R3)     TEST IF READ/DELETE NUMBERS ARE =            
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (P8,0(R3)),(15,15(R2)),2,MINUS=YES                               
         BAS   RE,WRITEIT                                                       
*                             SET DOLLAR DATA TO SUMREC SAVE                    
         L     R3,ADRSUMRC         GET ADDRES OF CURRENT SUMREC                 
         LA    R4,UNTORD                                                        
         LA    R1,RUNTOTS                                                       
         AP    0(8,R1),8(8,R4)                                                  
         EDIT  (P8,8(R4)),(15,30(R3)),2,MINUS=YES    UNTORD2                    
         LA    R4,UNTASSGN                                                      
         LA    R1,RUNTOTS                                                       
         AP    8(8,R1),8(8,R4)                                                  
         EDIT  (P8,8(R4)),(15,47(R3)),2,MINUS=YES     ASSIGNED2                 
         LA    R4,UNTBILL                                                       
         LA    R1,RUNTOTS                                                       
         AP    16(8,R1),8(8,R4)                                                 
         EDIT  (P8,8(R4)),(15,64(R3)),2,MINUS=YES    UNTBILL2                   
         LA    R4,UNTPAID                                                       
         LA    R1,RUNTOTS                                                       
         AP    24(8,R1),8(8,R4)                                                 
         EDIT  (P8,8(R4)),(15,81(R3)),2,MINUS=YES     UNTPAID2                  
*                                                                               
         L     R2,ADISKSV             CLEAR UNIT DISK ADDR                      
BRK10    OC    0(5,R2),0(R2)                                                    
         BZ    BRKX                                                             
         XC    0(5,R2),0(R2)                                                    
         LA    R2,5(R2)                                                         
         B     BRK10                                                            
*                                                                               
BRKX     XIT1                                                                   
*                                                                               
WRITEIT  NTR1                                                                   
         CLI   DELETESW,C'S'       SUMMARY ONLY                                 
         BE    BRKX                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     BRKX                                                             
         SPACE                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
********************************************                                    
* READ ESTIMATE HEADER                                                          
* OUTUT / SETS ESTKEYSV,ESTBSV                                                  
*                                                                               
         SPACE                                                                  
GETEST   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GEST**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
GPN1     MVI   ESTHDERR,0                                                       
         SPACE                                                                  
*  TO GET 3 CHAR PRD CODE FROM C LIST                                           
*  OUTPUT  PRDCODE IN WORK                                                      
         L     R2,ACLISTSV                                                      
GPRD1    CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GPRD2                                                            
         MVC   WORK(3),=C'UNA'   .SET TO UNDEFINED                              
         B     GPRD6                                                            
GPRD2    CLC   3(1,R2),NBPRD                                                    
         BE    *+12                                                             
         LA    R2,4(R2)                                                         
         B     GPRD1                                                            
         MVC   WORK(3),0(R2)      SET 3 CHAR PRINTABLE PRD CODE                 
*                                                                               
GPRD6    DS    0H                                                               
         LA    R3,KEY                                                           
         USING EKEY,R3                                                          
         XC    KEY,KEY                                                          
         MVC   EKEYAM,NBACTAM                                                   
         MVC   EKEYCLT,NBACTCLI                                                 
         MVC   EKEYPRD,WORK                                                     
         CLC   EKEYPRD,=C'UNA'                                                  
         BNE   *+10                                                             
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,NBACTEST                                                 
         NETGO NVSETSPT,DMCB          SET TO READ SPOT RECORDS                  
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    GPN4                                                             
*                                                                               
         MVI   ESTHDERR,C'Y'             NO ESTIMATE HEADER (ERROR)             
         MVI   CALLDDS,C'Y'                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVC   ESTBSV,NBACTEST                                                  
         MVC   P(45),=C'***** ERROR - NO ESTIMATE HEADER FOR UNIT ***'          
         MVC   P2(21),=C'ESTIMATE HEADER KEY ='                                 
         GOTO1 HEXOUT,DMCB,KEYSAVE,P2+22,13                                     
         GOTO1 =V(CLUNPK),DMCB,KEYSAVE+2,P3+24                                  
         MVC   P3+28(3),KEYSAVE+4                                               
         ZIC   R1,KEYSAVE+7                                                     
         EDIT  (R1),(3,P3+32)                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   P3,0                                                             
         GOTO1 HEXOUT,DMCB,NBKEY,P1,32     PXZ P4 CHANGETO P1                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         NETGO NVSETUNT,DMCB         SET TO READ UNIT RECORDS                   
         XC    FILENAME,FILENAME                                                
         MVI   NBSELPST,C'B'             LOCKED AND UNLOCKED                    
GETNXT   NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   GPERRX                                                           
         CLC   ESTBSV,NBACTEST                                                  
         BNE   GPN1                                                             
         GOTO1 HEXOUT,DMCB,NBKEY,P,32                                           
         GOTO1 DATCON,DMCB,(2,NBKEY+17),(5,P+65)                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     GETNXT                                                           
GPERRX   XC    ESTKEYSV(14),ESTKEYSV     SET STATUS TO NO DATA                  
         B     GPXIT                                                            
*                                                                               
GPN4     MVC   FILENAME,=C'SPTFIL  '                                            
         MVC   AIO,ANETWS1                                                      
         GOTO1 GETREC                                                           
*        GOTO1 HEXOUT,DMCB,KEY,P+10,20                                          
*        MVC   P(4),=C'EST '                                                    
*        GOTO1 HEXOUT,DMCB,AIO,P2,100                                           
*        GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R3,AIO                                                           
         USING ESTHDR,R3                                                        
         CLC   REQSTEND,EEND    TEST EST ENDDATE VS REQUEST ENDDATE             
         BNL   GPN5                                                             
         MVI   NBFUNCT,NBFNXEST                                                 
         B     GPNX                                                             
GPN5     MVC   NEWESTK,0(R3)       SET EST KEY                                  
         MVC   NEWESTB,EKEYEST       SET EST                                    
         SPACE                                                                  
GPNX     NETGO NVSETUNT,DMCB       RESET TO UNIT FILE                           
         XC    FILENAME,FILENAME   CLEAR FILENAME                               
*        L     RF,NBAIO              MOVE UNIT REC BACK TO NBAIO                
*        L     RE,ANETWS1                                                       
*        LA    R1,1000                                                          
*        MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
GPXIT    XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
GETCLST  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GETC**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NBACTAM                                                 
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   FILENAME,=C'SPTDIR  '                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FILENAME,=C'SPTFIL  '                                            
         L     RE,NBAIO                                                         
         L     RF,ANETWS1                                                       
         LA    R1,1000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         MVC   FULL,AIO            SAVE AIO                                     
         MVC   AIO,NBAIO                                                        
         GOTO1 GETREC                                                           
         MVC   AIO,FULL            RESET AIO                                    
         L     R2,NBAIO                                                         
         USING CLTHDR,R2                                                        
         MVC   BYTE,COFFICE        SAVE OFFICE CODE                             
         L     RF,ACLISTSV                                                      
         MOVE  ((RF),880),CLIST                                                 
         DROP  R2                                                               
         NETGO NVSETUNT,DMCB                                                    
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         L     RF,NBAIO                                                         
         L     RE,ANETWS1                                                       
         LA    R1,1000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
***************************************                                         
* READS/WRITES/CLOSES OUT GOAL RECORDS                                          
*                                                                               
GOALDNR  DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**GDNR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
*                                                                               
         MVI   ESTADD,0                                                         
         MVI   SWIT,0                                                           
         MVC   AIO,ANETWS1        SET I/O AREA FOR SPOT RECORDS                 
         L     R3,AIO                                                           
         USING GOALREC,R3                                                       
         NETGO NVSETSPT,DMCB                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+1(3),ESTKEYSV+1        A/M/CLT                               
         MVC   KEY+7(1),ESTKEYSV+7        ESTIMATE                              
         MVC   FILENAME,=C'SPTDIR  '                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     GL7                                                              
GOLSEQ   MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
GL7      CLC   KEY(4),KEYSAVE             ID/A/M/CLT                            
         BNE   GLXSP                                                            
         CLC   KEY+7(1),KEYSAVE+7          ESTIMATE                             
         BNE   GOLSEQ                                                           
         TM    KEY+11,GKEYTAR             PLANNED GOAL?                         
         BO    *+12                                                             
         CLI   KEY+11,0                   MUST BE ACTIVE POINTER                
         BNE   GOLSEQ                                                           
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         CLI   DELETESW,C'Y'                                                    
         BNE   GOLWRT                                                           
         OI    GCNTRLS,X'C0'                                                    
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+14,AIO,(0,DMWORK)           
         OI    KEY+13,X'C0'                                                     
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',FILENAME,KEY,KEY                          
GOLWRT   DS    0H                                                               
         LA    R2,P                                                             
         USING BPLINED,R2                                                       
         CLI   ESTADD,1                                                         
         BE    GL12                                                             
         BAS   RE,SPOLIT           SKIP LINE                                    
         MVI   ESTADD,1                                                         
         MVC   P(12),=C'GOAL RECORDS'                                           
         BAS   RE,SPOLIT                                                        
GL12     DS    0H                                                               
****     XC    P,P                                                              
         MVI   P,X'40'                                                          
         MVC   P+1(L'P-1),P                                                     
         GOTO1 =V(CLUNPK),DMCB,KEY+2,BPCLT                                      
         MVC   WORK(1),KEY+4                                                    
         BAS   RE,GTPRD                                                         
         MVC   BPPRD,WORK+10                                                    
         EDIT  (B1,KEY+7),(3,BPEST)                                             
         MVC   BPMNSRV(1),KEY+8               DAYPART                           
         EDIT  (B1,KEY+9),(3,BPYRSRV)       SPOT LENGTH                         
         BAS   RE,SPOLIT                                                        
         B     GOLSEQ                                                           
*                                                                               
GLXSP    MVI   ESTADD,0           PROCESS XSPOT GOAL RECORDS                    
         MVI   SWIT,0                                                           
         MVC   AIO,ANETWS1        SET I/O AREA FOR XSPOT RECORDS                
         L     R3,AIO                                                           
         USING GOALREC,R3                                                       
*                                                                               
         MVC   LKEY,=H'32'         FOR XSPOT RECORDS                            
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'42'                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+1(3),ESTKEYSV+1        A/M/CLT                               
         MVC   KEY+7(1),ESTKEYSV+7        ESTIMATE                              
         MVC   FILENAME,=C'XSPDIR  '                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     GLX7                                                             
GOLXSEQ  MVC   FILENAME,=C'XSPDIR  '                                            
         GOTO1 SEQ                                                              
GLX7     CLC   KEY(4),KEYSAVE             ID/A/M/CLT                            
         BNE   GOLX                                                             
         CLC   KEY+7(1),KEYSAVE+7          ESTIMATE                             
         BNE   GOLXSEQ                                                          
         TM    KEY+11,GXKEYTAR            PLANNED GOAL?                         
         BO    *+12                                                             
         CLI   KEY+11,0                   MUST BE ACTIVE POINTER                
         BNE   GOLXSEQ                                                          
         MVC   FILENAME,=C'XSPFIL  '                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+36,AIO,(0,DMWORK)           
         CLI   DELETESW,C'Y'                                                    
         BNE   GOLXWRT                                                          
         OI    GXRCNTRL,X'C0'                                                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,KEY+36,AIO,(0,DMWORK)           
         OI    KEY+32,X'C0'                                                     
         MVC   FILENAME,=C'XSPDIR  '                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',FILENAME,KEY,KEY                          
GOLXWRT  DS    0H                                                               
         LA    R2,P                                                             
         USING BPLINED,R2                                                       
         CLI   ESTADD,1                                                         
         BE    GLX12                                                            
         BAS   RE,SPOLIT           SKIP LINE                                    
         MVI   ESTADD,1                                                         
         MVC   P(12),=C'GOAL RECORDS'                                           
         BAS   RE,SPOLIT                                                        
GLX12    DS    0H                                                               
****     XC    P,P                                                              
         MVI   P,X'40'                                                          
         MVC   P+1(L'P-1),P                                                     
         GOTO1 =V(CLUNPK),DMCB,KEY+2,BPCLT                                      
         MVC   WORK(1),KEY+4                                                    
         MVC   BPPRD,KEY+13                                                     
         EDIT  (B1,KEY+7),(3,BPEST)                                             
         MVC   BPMNSRV(1),KEY+8               DAYPART                           
         EDIT  (B1,KEY+9),(3,BPYRSRV)       SPOT LENGTH                         
         BAS   RE,SPOLIT                                                        
         B     GOLXSEQ                                                          
*                                                                               
GOLX     DS    0H                                                               
         MVC   AIO,NBAIO         RESET NETIO I/O AREA                           
         NETGO NVSETUNT,DMCB                                                    
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         MVI   SWIT,0                                                           
         MVI   ESTADD,0                                                         
         XIT1                                                                   
         DROP  R2,R3                                                            
*                                                                               
SPOLIT   NTR1                                                                   
         CLI   DELETESW,C'S'                                                    
         BE    SX                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
SX       XIT1                                                                   
*                                                                               
********************************************                                    
*  TO GET 3 CHAR PRD CODE FROM C LIST                                           
*  INPUT   WORK HAS PRDNO                                                       
*  OUTPUT  PRDCODE IN WORK+10                                                   
*                                                                               
GTPRD    NTR1                                                                   
         L     R2,ACLISTSV                                                      
GT10     CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GT12                                                             
         MVC   WORK+10(3),=C'UNA'   .SET TO UNDEFINED                           
         B     GTPX                                                             
GT12     CLC   3(1,R2),WORK                                                     
         BE    GT14                                                             
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GT10                RETURN TO LOOP                               
GT14     MVC   WORK+10(3),0(R2)      SET 3 CHAR PRINTABLE PRD CODE              
GTPX     XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
HISTRY   DS    0H                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**HSTR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
*                                  DEAL WITH HISTORY RECORDS                    
         L     R1,NBAIO                                                         
         LA    R1,27(R1)                                                        
         CLI   0(R1),X'01'             01 ELEMENT                               
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R1                                                      
         TM    NUPACKST,X'02'          HISTROY ON?                              
         BNO   HISTX                                                            
         DROP  R1                                                               
         LA    R1,KEY                                                           
         USING NHRECD,R1                                                        
         XC    KEY+4(16),KEY+4                                                  
         MVI   KEY,X'40'                                                        
         MVC   NHKNET,NBACTNET                                                  
         MVC   NHKPROG,NBACTPRG                                                 
         MVC   NHKDATE,NBACTDAT                                                 
         MVC   NHKEST,NBACTEST                                                  
         MVC   NHKSUB,NBACTSUB                                                  
         MVC   NHKDP,NBACTDP                                                    
         MVC   FILENAME,=C'UNTDIR  '                                            
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE         IF NOT THERE SKIP                        
         BNE   HISTX                                                            
         MVC   FILENAME,=C'UNTFIL  '                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+21,NBAIO,(0,DMWORK)         
         L     R1,NBAIO                                                         
         OI    22(R1),X'C0'                                                     
         LA    R2,KEY+21                                                        
         GOTO1 DATAMGR,DMCB,=C'PUTREC',FILENAME,0(R2),NBAIO,(0,DMWORK)          
*                                                                               
         MVC   FILENAME,=C'UNTDIR  '                                            
         GOTO1 HIGH                                                             
         OI    KEY+20,X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',FILENAME,KEY,KEY                          
         L     R1,HISTCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,HISTCNT                                                       
HISTX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
QRECORD  DSECT                      SPONSOR STYLE REQUEST CARD                  
QAREA    DS    0CL80   COLUMN                                                   
QPROG    DS    0CL2    ------                                                   
QCODE    DS    CL2        1        PROGRAM CODE                                 
QAGY     DS    CL2        3        AGENCY CODE                                  
QMED     DS    CL1        5        MEDIA CODE (R/T)                             
QCLT     DS    CL3        6        CLIENT CODE                                  
QPGR     DS    CL1        9        PROCESS BY DIVISION                          
QMGR     DS    CL1       10        PROCESS BY DISTRICT                          
QCLOFFC  DS    CL1       11        CLIENT OFFICE FILTER                         
QBYID    EQU   QCLOFFC             C'Y' IF BUYS PROCESSED BY ID                 
QPRD     DS    CL3       12        PRODUCT MNEMONIC                             
QMKT     DS    CL4       15        MARKET NUMBER                                
QSTA     DS    CL5       19        STATION CALL LETTERS                         
QEST     DS    CL3       24        ESTIMATE NUMBER                              
QESTEND  DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
QDEMOVRD DS    CL1       30        Y=DEMO OVERRIDE ACTIVE                       
QCONTREQ DS    CL1       31        C'*' ==> DATA IN QAREA2                      
QSTAUTO  DS    CL3       32        AUTO REQUEST START DATE                      
QENDAUTO DS    CL3       35        AUTO REQUEST END DATE                        
         ORG   QSTAUTO+2                                                        
QDEMNOS  DS    CL4                 DEMO OVERRIDE NUMBERS                        
QSTART   DS    CL6       38        REQUEST START DATE                           
QEND     DS    0CL6      44        REQUEST END DATE                             
QTODAY   DS    CL6       44                                                     
QBOOK1   DS    CL4       50        RATING BOOK (YYMM) GOAL/ESTD DATA            
QHUT1    DS    CL2       54        HUT ADJUSTMENT MONTH                         
QRERATE  DS    CL1       56        RERATE TYPE  I=INVOICE                       
*                                               P=PURCHASED                     
*                                               A=ADJUST ONLY                   
*                                               U=UPGRADE (+Q2BOOK2)            
QCOMPARE DS    CL1       57        DATA COMPARE OPTION                          
*                                  A=GOAL V PURCHASED                           
*                                  B=GOAL V AFFIDAVIT                           
*                                  C=PURCHASED V PURCHASED (RERATED)            
*                                  D=PURCHASED V AFFIDAVIT                      
*                                  E=LOCKIN V PURCHASED                         
*                                  F=LOCKIN V AFFIDAVIT                         
*                                  L=GOAL V PURCHASED, LOCKIN PURCHASED         
QAFFIL   DS    CL1       58        AFFILIATION FILTER                           
QPRGTYPE DS    CL1       59        PROGRAM TYPE FILTER                          
QDPTDET  DS    CL1       60        DAYPART DETAIL CONTROL                       
*                                  A=SHOW FULL DETAIL (DEFAULT)                 
*                                  B=SUPPRESS SPOT-LENGTH                       
*                                  C=SUPPRESS SPOT-LENGTH & DAY-PART            
QDPTMENU DS    CL1       61        DAYPART MENU OVERRIDE                        
QOPT1    DS    CL1       62        OPTION 1                                     
QOPT2    DS    CL1       63        OPTION 2                                     
QOPT3    DS    CL1       64        OPTION 3                                     
QOPT4    DS    CL1       65        OPTION 4                                     
QOPT5    DS    CL1       66        OPTION 5                                     
QGRP     DS    CL2       67        GROUP                                        
QFILTER  EQU   QGRP                FILTER TYPE/VALUE                            
QUESTOR  DS    CL12      69        REQUESTOR NAME                               
*                                                                               
       EJECT                                                                    
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS4                         
*                            * THESE ARE RECEIVED FROM EDIT MOFULE              
ESTKEYSV DS    CL13          *                                                  
ESTBSV   DS    CL1           *                                                  
KILLSW   DS    CL1           *                                                  
PUFSW    DS    CL1           *                                                  
DELETESW DS    CL1           *                                                  
OFFCDE   DS    CL1           *                                                  
REQSTEND DS    CL6           *                                                  
RELO     DS    F             *                                                  
ACLISTSV DS    F             *                                                  
*                                                                               
ACTVCLT  DS    CL3                 USED BY I8/TR REQUESTS                       
         DS    0F                                                               
*                                                                               
ASUMRECD DS    F                                                                
ADRSUMRC DS    F                                                                
ESTCNTR  DS    F                   COUNTER FOR ESTIMATE RECORDS                 
UNTCNTR  DS    F                   COUNTER FOR UNIT RECORDS                     
UNTCNTR2 DS    F                   COUNTER FOR UNIT RECORDS                     
BILLCNTR DS    F                   COUNTER FOR BILLING RECORDS                  
BILLCNT2 DS    F                   COUNTER FOR BILLING RECORDS                  
STABCNTR DS    F                                                                
STABCNT2 DS    F                   COUNTER FOR SPGENSTAB RECORDS                
SUMCNTR  DS    F                   COUNTER FOR SUMMARY RECAP RECS               
AP       DS    F                                                                
ABRKTOT  DS    F                                                                
AERRORS  DS    F                                                                
APKGDNR  DS    F                                                                
AGOALDNR DS    F                                                                
AGETCLST DS    F                                                                
AGETEST  DS    F                                                                
PKGCNTR  DS    F                   COUNTER FOR PACKAGE RECORDS                  
ADISKSV  DS    F                                                                
DSKSVLEN DS    F                   LENGTH OF DISK TABLE                         
*                                                                               
TBLMAXRC DS    F                    200000=MAX NUMBER OF RECS                   
TBLRCLEN DS    F                     5=LENGTH OF EACH REC                       
*                                                                               
HISTCNT  DS    F                                                                
UNTBLCNT DS    F                                                                
*                                                                               
                                                                                
NEWESTK  DS    CL13                                                             
NEWESTB  DS    CL1                                                              
*                                                                               
ERRORSW  DS    CL1                                                              
UNTWARN  DS    CL1                                                              
ESTHDOPT DS    CL1                                                              
FRST     DS    CL1                                                              
SWIT     DS    CL1                                                              
BOXSET   DS    CL1                                                              
PAIDSW   DS    CL1                                                              
BILLSW   DS    CL1                                                              
BPRDSV   DS    CL1                                                              
PRDSV    DS    CL3                                                              
ESTBYRS  DS    CL1                 ESTIMATE START YEAR                          
ESTBMNS  DS    CL1                 ESTIMATE START MONTH                         
ESTBYRE  DS    CL1                 ESTIMATE END YEAR                            
ESTBMNE  DS    CL1                 ESTIMATE END MONTH                           
BYEAR    DS    CL1                 REQUEST END YEAR                             
BMNTH    DS    CL1                 REQUEST END MONTH                            
ASSIGNB  DS    CL1                 BILLING ON ASIGNED                           
ESTWARN  DS    CL4                                                              
NXTOFFCD DS    CL1                                                              
NEWCLI   DS    CL1                                                              
ESTHDERR DS    CL1                                                              
ESTADD   DS    CL1                                                              
CALLDDS  DS    CL1                                                              
UNITKSV  DS    CL20                                                             
*                                                                               
*                   TOTALS TAKEN FIRST TIME RECS ARE READ                       
*                     UNITS                                                     
UNTORD   DS    PL8       ORDERED UNIT TOTAL                                     
UNTORD2  DS    PL8       ORDERED UNIT TOTAL AT DELETE TIME                      
UNTBILL  DS    PL8       BILLED UNIT TOTAL                                      
UNTBILL2 DS    PL8       BILLED UNIT TOTAL AT DELETE TIME                       
UNTPAID  DS    PL8       PAID UNIT TOTAL                                        
UNTPAID2 DS    PL8       PAID UNIT TOTAL AT DELETE TIME                         
UNTASSGN DS    PL8                                                              
UNTASSG2 DS    PL8                                                              
*                     BILL RECS                                                 
BLRECAMT DS    PL8       BILL REC AMOUNT TOTAL                                  
BLRECAM2 DS    PL8       BILL REC AMOUNT TOTAL AT DELETE TIME                   
*                     SPGENSTAB                                                 
STABAMT  DS    PL8           SPGENSTAB TOTAL                                    
STABAMT2 DS    PL8           SPGENSTAB TOTAL AT DELETE TIME                     
*                                                                               
RUNTOTS  DS    PL8   *****  TOTALS FOR CLIENT RUN  ****                         
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
TOTEST   DS    PL8                                                              
TOTPKG   DS    PL8                                                              
TOTUNT   DS    PL8                                                              
TOTBILL  DS    PL8                                                              
TOTSTAB  DS    PL8                                                              
FNLTOTS  DS    PL8   *****  TOTALS FOR ENTIRE RUN  ****                         
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
FNLEST   DS    PL8                                                              
FNLPKG   DS    PL8                                                              
FNLUNT   DS    PL8                                                              
FNLBILL  DS    PL8                                                              
FNLSTAB  DS    PL8                                                              
TOTNETB  DS    PL8                                                              
TOTNETP  DS    PL8                                                              
FNLNETB  DS    PL8                                                              
FNLNETP  DS    PL8                                                              
         EJECT                                                                  
*                                                                               
AGYTOTD  DSECT                                                                  
AGYTOTS  DS    PL8   *****  TOTALS FOR ACROSS AGY RUN      *****                
         DS    PL8   *****  IN CASE OF MULTIPLE REQS FOR SAME AGY ***           
         DS    PL8                                                              
         DS    PL8                                                              
AGYEST   DS    PL8                                                              
AGYPKG   DS    PL8                                                              
AGYUNT   DS    PL8                                                              
AGYBILL  DS    PL8                                                              
AGYSTAB  DS    PL8                                                              
AGYNETB  DS    PL8                                                              
AGYNETP  DS    PL8                                                              
MULTREQ  DS    CL1                                                              
ADISKSVV DS    F                                                                
*                                                                               
BPLINED  DSECT               BILL REC DSECT FOR PRINTING                        
         DS    CL1                                                              
BPCLT    DS    CL3                                                              
         DS    CL1                                                              
BPPRD    DS    CL3                                                              
         DS    CL1                                                              
BPEST    DS    CL3                                                              
         DS    CL1                                                              
BPMNSRV  DS    CL2                                                              
         DS    CL1                                                              
BPYRSRV  DS    CL2                                                              
         DS    CL4                                                              
BPINVO   DS    CL6                                                              
         DS    CL4                                                              
BPDATE   DS    CL6                                                              
         DS    CL4                                                              
BPAMT    DS    CL11                                                             
         EJECT                                                                  
*                                                                               
UPLINED  DSECT               UNIT REC DSECT FOR PRINTING                        
         DS    CL1                                                              
UPCLT    DS    CL3                                                              
         DS    CL1                                                              
UPNET    DS    CL4                                                              
         DS    CL1                                                              
UPPRD    DS    CL3                                                              
         DS    CL1                                                              
UPPAK    DS    CL3                                                              
         DS    CL1                                                              
UPPROG   DS    CL6                                                              
         DS    CL1                                                              
UPDATE   DS    CL8                                                              
         DS    CL1                                                              
UPSUBLN  DS    CL6                                                              
         DS    CL1                                                              
UPACTUAL DS    CL12                ORDERED                                      
         DS    CL1                                                              
UPINTEG  DS    CL12                INTEGRATION                                  
         DS    CL1                                                              
UPASSGN  DS    CL12                ASSIGNED                                     
         DS    CL1                                                              
UPBILT   DS    CL12                BILLED TIME                                  
         DS    CL1                                                              
UPBILI   DS    CL12                BILLED INTEGRATION                           
         DS    CL1                                                              
UPPAIDT  DS    CL12                PAID TIME                                    
         DS    CL1                                                              
UPPAIDI  DS    CL12                PAID INTEGRATION                             
         EJECT                                                                  
ESVRECD  DSECT       *** NOT USED*** DESECT FOR SAVED ESTIMATE INFO             
ESVCLT   DS    CL3                                                              
ESVPRD   DS    CL3                                                              
ESVEST   DS    CL3                                                              
ESVDATE  DS    CL13                                                             
ESVLNE   EQU   *-ESVCLT                                                         
         EJECT                                                                  
         SPACE                                                                  
         PRINT ON                                                               
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDDAD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDTWADCOND                                                     
*                                                                               
         EJECT                                                                  
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEGENHIST                                                      
       ++INCLUDE NEGENUBILL                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055NEMEDCB   08/19/11'                                      
         END                                                                    

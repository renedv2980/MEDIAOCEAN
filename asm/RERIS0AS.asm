*          DATA SET RERIS0AS   AT LEVEL 019 AS OF 05/01/02                      
*          DATA SET RERIS0A    AT LEVEL 078 AS OF 02/20/98                      
*PHASE T80D0AC,+0                                                               
*INCLUDE REPIOA                                                                 
         TITLE 'RIS - T80D0A - READING MODULE'                                  
*                                                                               
*- RERIS0A -- PHASE T80D0A                                                      
*                                                                               
***********************************************************************         
*  HISTORY OF CHANGES:                                                          
***********************************************************************         
* NOV01/96 (BU ) ---  'OTHER' STATIONS (NOT ACE/GRAPHNET) ARE TO BE   *         
*                     TREATED AS 'CONFIRMED'                          *         
*                                                                     *         
* <*** DARK TIMES ***>                                                *         
*                                                                     *         
* AUG01/97 (JRD) --- NEW OPTIONS(D, -D, DS=, DT=)                     *         
*                                                                     *         
* AUG08/97 (JRD) --- SETS AND STATION MARKET                          *         
*                                                                     *         
* OCT24/97 (BU ) --- RERISWRKB --> RERISWRKC                          *         
*                    RGENEROL INCLUDED EXPLICITLY                     *         
*                                                                     *         
* DEC10/98 (BU ) --- S/P-TEAM FILTERING                               *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                     *** END TOMBSTONE ***                           *         
***********************************************************************         
*                                                                     *         
* NOTE: AIOAREA IS REMAPPED TO "RECORD" IN THE 02 05 & 06             *         
*        IOAREA IS NOT BIG ENOUGH FOR A CONTRACT.                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80D0A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0D0A*                                                        
         USING T80D0A,RB,R7                                                     
         LA    R7,2048(RB)              NOTE R7 BASE REGISTER                   
         LA    R7,2048(R7)                                                      
                                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T80DFFD,RA                                                       
                                                                                
         CLI   LISTBYTE,0          IF ONLY $ READ                               
         BE    OLDRIS              USE OLD RIS KEYS                             
         CLI   LISTBYTE,LISTPNAM   IF PRODNAME LIST ORDER                       
         BE    PNAMRDR             USE X'9D' KEY READ                           
                                                                                
* NEW RIS USING 8D AND 8E PASSIVE KEYS                                          
         LA    R2,REPIOTBL                                                      
         USING REPIOD,R2                                                        
         MVC   RIPDTMGR,VDATAMGR                                                
         MVC   RIPIOARE,AIOAREA                                                 
         OI    RIPSTAT,RIPRDHI                                                  
         DS    0H                                                               
READREP  GOTO1 =V(REPIO),DMCB,REPIOTBL,RR=Y                                     
                                                                                
         TM    RIPSTAT,RIPRQERR              ANY REQUIRED DATA MISSING          
         BZ    *+6                                                              
         DC    H'0'                          SHOULD NEVER GET HERE              
                                                                                
         TM    RIPSTAT,RIPMAXIO    MAX IO ERROR                                 
         BZ    *+12                                                             
         MVI   DMCB+1,X'FF'        YES                                          
         B     RDRX                                                             
                                                                                
         TM    RIPSTAT,RIPENDF     END OF FILE                                  
         BZ    *+12                                                             
         MVI   DMCB,X'FF'          YES                                          
         B     RDRX                                                             
                                                                                
* SET MAXIOCTR TO 90% OF MAX IO'S SHOWN BY GETFACT                              
         OC    MAXIOCTR,MAXIOCTR   IS MAXIOCTR SET ?                            
         BNZ   *+12                YES                                          
         BAS   RE,SETMAXCT         SET MAX I/O COUNTER                          
         B     *+8                                                              
         BAS   R4,CHKMAXCT         CHECK MAX I/O COUNTER                        
                                                                                
* FILTERS                                                                       
* CONTRACT STATUS ROUTINES                                                      
RDR03    DS    0H                                                               
         BAS   RE,FILTERS                                                       
         BNE   READREP                                                          
         BE    RDR10                                                            
                                                                                
RDR10    EQU   *                                                                
                                                                                
RDR20    EQU   *                                                                
*                                                                               
                                                                                
RDRX     XIT1                                                                   
                                                                                
NOTOK    B     READREP                                                          
*                                                                               
         EJECT                                                                  
* READ CONTRACT RECS USING X'9D' PASSIVE KEYS                                   
*                                                                               
PNAMRDR  DS    0H                                                               
                                                                                
         OC    MAXIOCTR,MAXIOCTR   IS MAXIOCTR SET ?                            
         BNZ   *+12                YES                                          
         BAS   RE,SETMAXCT         SET MAX I/O COUNTER                          
         B     *+8                                                              
         BAS   R4,CHKMAXCT         CHECK MAX I/O COUNTER                        
                                                                                
         MVI   ERRAREA,0                                                        
                                                                                
         LA    R2,REPIOTBL         USE REPIO BLOCK FOR FILTERING                
         USING REPIOD,R2                                                        
*                                                                               
         TM    RIPSTAT,RIPRDHI+RIPRDHIO     READ HI BEFORE SEQ                  
         BZ    PNAM05                                                           
         MVC   KEY,RIPKEY                                                       
         BAS   RE,HIGH                                                          
         TM    RIPSTAT,RIPRDHIO    READ HI ONLY ?                               
         BZ    PNAM03                                                           
         MVI   RIPSTAT,0           YES/CLEAR FLAG                               
         B     PNAM20              PROCESS                                      
                                                                                
PNAM03   EQU   *                   NO/READ HI BEFORE SEQ                        
         MVI   RIPSTAT,0           CLEAR FLAG                                   
         B     PNAMSEQ             GO DO SEQ READ                               
*                                                                               
PNAM05   CLI   RIPKEY,0            IF 0                                         
         BE    PNAM06              START READ                                   
                                                                                
         CLI   RIPKEY,X'9D'        NOT X'9D' KEY ?                              
         BNE   PNAM06              NO - ASSUME FIRST TIME                       
                                                                                
         MVC   KEY,RIPKEY          YES - X'9D' RESUME SEQ READ                  
         B     PNAMSEQ                                                          
*                                                                               
PNAM06   XC    KEY,KEY             FIRST TIME                                   
         LA    R3,KEY                                                           
         USING RCON9DK,R3                                                       
         MVI   RCON9DTP,X'9D'                                                   
         MVC   RCON9DRP,RIPREP                                                  
         CLI   RIPSTA,X'40'          STATION                                    
         BNH   *+10                                                             
         MVC   RCON9DST,RIPSTA                                                  
         CLI   RIPADV,X'40'           ADVERTISER                                
         BNH   *+10                                                             
         MVC   RCON9DDV,RIPADV                                                  
                                                                                
PNAMHI   BAS   RE,HIGH                                                          
         B     PNAM20                                                           
         DROP  R3                                                               
*                                                                               
PNAMSEQ  BAS   RE,SEQ                                                           
*                                                                               
PNAM20   CLC   KEY(3),KEYSAVE      ID/REP ?                                     
         BE    PNAM22              OK                                           
PNAM21   MVI   DMCB,X'FF'          END OF FILE                                  
         B     PNMRDRX                                                          
                                                                                
PNAM22   CLI   RIPSTA,X'40'        FILTERING ON STATION ?                       
         BNH   PNAM25                                                           
         CLC   KEY+3(5),RIPSTA     YES/CHECK THAT HERE                          
         BNE   PNAM21                                                           
*                                                                               
PNAM25   DS    0H                                                               
         BAS   RE,PNAMKFLT         FILTER X'9D' KEY                             
         BNE   PNAMSEQ                                                          
                                                                                
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         BAS   RE,GETREC                                                        
         NI    DMINBTS,X'FF'-X'08' TURN OFF BIT                                 
         L     R4,AIOAREA                                                       
         USING RCONREC,R4                                                       
         TM    RCONCNTL,X'80'      DELETED ?                                    
         BO    PNAMSEQ                                                          
         DROP  R4                                                               
                                                                                
         BAS   R4,CHKMAXCT         CHECK MAX I/O COUNTER                        
         BAS   RE,PNAMRFLT         FILTER CONTRACT                              
         BNE   PNAMSEQ                                                          
         BAS   RE,FILTERS          COMMON FILTS TO REPIO/9D/0C READ             
         BNE   PNAMSEQ                                                          
         BAS   RE,SPECFLT          SPECIAL FILTERS                              
         BNE   PNAMSEQ                                                          
         MVC   RIPKEY,KEY          SAVE CURRENT CONTRACT KEY                    
                                                                                
PNMRDRX  XIT1                                                                   
                                                                                
         EJECT                                                                  
* IN X'9D' READ, FILTER CONTRACTS AGAINST REPIOBLK                              
*                                                                               
PNAMRFLT NTR1                                                                   
         L     R4,AIOAREA                                                       
         USING RCONREC,R4                                                       
         CLC   RIPGRP,X'40'         GROUP                                       
         BNH   *+14                                                             
         CLC   RIPGRP,RCONKGRP                                                  
         BNE   PNMX                                                             
         CLC   RIPAGY,X'40'         AGENCY                                      
         BNH   *+14                                                             
         CLC   RIPAGY(4),RCONKAGY                                               
         BNE   PNMX                                                             
         CLC   RIPAGOFF,X'40'         AGENCY/OFF                                
         BNH   *+14                                                             
         CLC   RIPAGOFF,RCONKAOF                                                
         BNE   PNMX                                                             
         CLI   RIPOFF,X'40'         OFFICE                                      
         BNH   *+14                                                             
         CLC   RIPOFF,RCONKOFF                                                  
         BNE   PNMX                                                             
         CLI   RIPSAL,X'40'        SALESMAN                                     
         BNH   *+14                                                             
         CLC   RIPSAL,RCONSAL                                                   
         BNE   PNMX                                                             
         CLC   RIPCON,X'40'        CONTRACT TYPE                                
         BNH   *+14                                                             
         CLC   RIPCON,RCONTYPE                                                  
         BNE   PNMX                                                             
                                                                                
         CLC   RIPREP,RIPREP       SET CC =                                     
*                                                                               
         DROP  R4                                                               
PNMX     XIT1                                                                   
         EJECT                                                                  
* CHECK X'9D' KEY AGAINST REPIOBLK                                              
PNAMKFLT NTR1                                                                   
         LA    R3,KEY                                                           
         USING RCON9DK,R3                                                       
         CLI   RIPSTA,X'40'        STATION FILTER                               
         BNH   *+14                                                             
         CLC   RIPSTA,RCON9DST                                                  
         BNE   PNKX                                                             
         CLI   RIPADV,X'40'        ADVERTISER FILTER                            
         BNH   *+14                                                             
         CLC   RIPADV,RCON9DDV                                                  
         BNE   PNKX                                                             
         CLC   RIPREP,RIPREP        SET CC =                                    
PNKX     XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
OLDRIS   DS    0H                                                               
*                                                                               
* SET MAXIOCTR TO 90% OF MAX IO'S SHOWN BY GETFACT                              
*                                                                               
         LR    R3,R1               SAVE R1                                      
         OC    MAXIOCTR,MAXIOCTR   IS MAXIOCTR SET ?                            
         BNZ   *+12                YES                                          
         BAS   RE,SETMAXCT         SET MAX I/O COUNTER                          
         B     *+8                                                              
         BAS   R4,CHKMAXCT         CHECK MAX I/O COUNTER                        
         LR    R1,R3               RESTORE R1                                   
*                                                                               
         L     R8,SAVER8                                                        
         SR    R5,R5                                                            
         L     R5,4(R1)            ZERO ON FIRST TIME ONLY                      
         L     R5,0(R5)                                                         
         B     BRANCH(R5)                                                       
         SPACE 1                                                                
BRANCH   B     ROUTER                                                           
         B     SQKEY0C                                                          
         B     SQKEY9C                                                          
         B     SQKEYAC                                                          
         B     SQKEYBC                                                          
         EJECT                                                                  
*                                  DETERMINE READING ROUTINE                    
ROUTER   CLC   TBLSLS,ALL          AC KEY READ ON ANY SALESMAN OR TEAM          
         BNE   RTR8                                                             
         CLC   TBLTEAM,ALL                                                      
         BNE   RTR8                                                             
         CLC   TBLDIV,ALL                                                       
         BE    RTR9                                                             
RTR8     LA    R0,12                                                            
         ST    R0,LINK                                                          
         B     SETKEYAC                                                         
*                                                                               
RTR9     CLC   TBLCLS,ALL                                                       
         BNE   RTR9A                                                            
         CLC   TBLCTG,ALL                                                       
         BE    RTR10                                                            
*                                                                               
RTR9A    LA    R0,16               READ CATEGORY KEY                            
         ST    R0,LINK                                                          
         B     SETKEYBC                                                         
         SPACE 2                                                                
RTR10    CLC   TBLGRP,ALL          IF THERE IS A GROUP,                         
         BE    RTR18                                                            
         CLC   TBLOFF,ALL          BUT NO OFFICE                                
         BE    RTR20               READ 0C KEY                                  
*                                                                               
         CLC   TBLSTA,ALL          EVEN IF THERE IS AN OFFICE, BUT NO           
         BE    RTR20               STATION,READ 0C KEY                          
*                                                                               
RTR12    CLC   TBLAGY,ALL          OR THERE IS AN OFFICE & AN AGENCY            
         BNE   RTR20               READ 0C KEY                                  
         CLC   TBLADV,ALL          OR NO AGENCY & NO ADVERTISER                 
         BE    RTR20               READ 0C KEY                                  
*                                                                               
RTR14    LA    R0,8                ELSE, READ 9C KEY                            
         ST    R0,LINK                                                          
         B     SETKEY9C                                                         
RTR18    CLC   TBLOFF,ALL          THERE IS NO GROUP, TEST OFF                  
         BE    RTR12               IF NONE, MAKE AGY/ADV TEST AGAIN             
         B     RTR14                                                            
         SPACE 2                                                                
RTR20    LA    R0,4                READ 0C KEY                                  
         ST    R0,LINK                                                          
         B     SETKEY0C                                                         
         EJECT                                                                  
SETKEYAC XC    KEY,KEY             SET KEY FOR DMRDHI                           
         MVI   KEY,X'AC'                                                        
         MVC   KEY+1(2),REPALPHA   BUILD UNTIL FIRST ALL                        
         LA    R8,2                SET MANDATORY COMPARE LENGTH-1               
         CLC   TBLOFF,ALL          TEST ALL OFFICE                              
         BE    RDKEYAC                                                          
         MVC   KEY+3(2),TBLOFF     BUILD MORE KEY - OFFICE                      
         LA    R8,4                                                             
*                                                                               
         CLC   TBLDIV,ALL          TEST DIVISION                                
         BE    RDKEYAC                                                          
         MVC   KEY+5(1),TBLDIV                                                  
         LA    R8,5                                                             
*                                                                               
         CLC   TBLTEAM,ALL         TEST TEAM                                    
         BE    RDKEYAC                                                          
         MVC   KEY+6(1),TBLTEAM                                                 
         LA    R8,6                                                             
*                                                                               
         CLC   TBLSLS,ALL          TEST SALESMAN                                
         BE    RDKEYAC                                                          
         MVC   KEY+7(3),TBLSLS                                                  
         LA    R8,9                                                             
*                                                                               
         CLC   TBLSTA,ALL          TEST STATION                                 
         BE    RDKEYAC                                                          
         MVC   KEY+10(5),TBLSTA                                                 
         LA    R8,14                                                            
*                                                                               
         CLC   TBLAGY,ALL          TEST AGENCY                                  
         BE    RDKEYAC                                                          
         MVC   KEY+15(4),TBLAGY                                                 
         LA    R8,18                                                            
*                                                                               
         CLC   TBLADV,ALL          TEST ADVERTISER                              
         BE    RDKEYAC                                                          
         MVC   KEY+19(4),TBLADV                                                 
         LA    R8,22                                                            
         SPACE 2                                                                
RDKEYAC  BAS   RE,HIGH                                                          
*                                                                               
         EX    R8,KEYCLC           TEST MANDATORY LENGTH                        
         BNE   KEYDONE                                                          
         MVC   RCONKEY,KEY                                                      
         B     TSTAC                                                            
         SPACE 2                                                                
SQKEYAC  BAS   RE,SEQ                                                           
*                                                                               
         EX    R8,KEYCLC                                                        
         BNE   KEYDONE                                                          
         MVC   RCONKEY,KEY                                                      
         B     TSTAC                                                            
*                                                                               
KEYCLC   CLC   KEY(0),KEYSAVE                                                   
         EJECT                                                                  
TSTAC    CLC   TBLDIV,ALL                                                       
         BE    TSTAC10                                                          
         CLC   TBLDIV,RCONRTEM                                                  
         BNE   NXTAC10                                                          
*                                                                               
TSTAC10  EQU   *                                                                
         CLC   TBLTEAM,ALL                                                      
         BE    TSTAC20                                                          
         CLC   TBLTEAM,RCONRTEM+1                                               
         BNE   NXTAC20                                                          
*                                                                               
TSTAC20  CLC   TBLSLS,ALL                                                       
         BE    TSTAC30                                                          
         CLC   TBLSLS,RCONRSAL                                                  
         BNE   NXTAC30                                                          
*                                                                               
TSTAC30  CLC   TBLSTA,ALL          TEST STATION                                 
         BE    TSTAC40                                                          
         CLC   RCONRSTA,TBLSTA                                                  
         BNE   NXTAC40                                                          
*                                                                               
TSTAC40  CLC   TBLAGY,ALL          TEST AGENCY                                  
         BE    TSTAC50                                                          
         CLC   RCONRAGY,TBLAGY                                                  
         BNE   NXTAC50                                                          
*                                                                               
TSTAC50  CLC   TBLADV,ALL          TEST ADVERTISER                              
         BE    TSTAC60                                                          
         CLC   RCONRADV,TBLADV                                                  
         BNE   NXTAC60                                                          
         SPACE 1                                                                
TSTAC60  OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,GETREC                                                        
         NI    DMINBTS,X'F7'                                                    
         TM    RCONCNTL,X'80'                                                   
         BO    SQKEYAC                                                          
         BAS   RE,FILTERS                                                       
         BNE   SQKEYAC                                                          
         CLC   TBLAOFF,ALL          TEST AGENCY-OFFICE                          
         BE    TSTAC62                                                          
         CLC   TBLAOFF,RCONKAOF                                                 
         BNE   SQKEYAC                                                          
*                                                                               
TSTAC62  CLC   TBLGRP,ALL          TEST STATION GROUP                           
         BE    TSTAC64                                                          
         CLC   TBLGRP,RCONKGRP                                                  
         BNE   SQKEYAC                                                          
*                                                                               
TSTAC64  CLC   TBLSBGP,ALL         TEST STATION-SUBGROUP                        
         BE    TSTAC66                                                          
         CLC   TBLSBGP,RCONKGRP+1                                               
         BNE   SQKEYAC                                                          
*                                                                               
TSTAC66  CLC   TBLCTG,ALL                                                       
         BE    TSTAC68                                                          
         CLC   TBLCTG,RCONCTGY                                                  
         BNE   SQKEYAC                                                          
*                                                                               
TSTAC68  CLC   TBLFILT,ALL                                                      
         BE    TSTAC69                                                          
         CLC   TBLFILT,RCONTYPE                                                 
         BE    TSTAC69                                                          
         TM    TBLFILT,X'40'                                                    
         BO    SQKEYAC             NOT A NEGATIVE FILTER                        
         NI    RCONTYPE,X'BF'                                                   
         CLC   TBLFILT,RCONTYPE                                                 
         BE    SQKEYAC             EXCLUDE                                      
         SPACE 1                                                                
TSTAC69  EQU   *                                                                
         MVC   DMCB+4(4),LINK                                                   
         ST    R8,SAVER8                                                        
         B     EXXMOD                                                           
         EJECT                                                                  
* NXTAC REBUILDS KEY ACCORDING TO OPTIONS EMPLOYED, THEN READS HIGH.            
* IT BACKS UP THROUGH THE FIELDS OF THE KEY,DETERMINING REASON KEY WAS          
* NOT VALID AND ATTEMPTING TO CORRECT IT.                                       
         SPACE 1                                                                
NXTAC60  XC    KEY+19(8),KEY+19    IF FIELD WAS ALL, KEY WOULD BE OK            
         CLC   RCONRADV,TBLADV     ADVERTISER FIELD                             
         BNL   NXTAC58                                                          
         MVC   KEY+19(4),TBLADV                                                 
         B     RDKEYAC                                                          
         SPACE 1                                                                
NXTAC58  CLC   TBLAGY,ALL          DOES AGENCY=ALL                              
         BNE   NXTAC50                                                          
*                                                                               
         IC    RE,KEY+18           INCREMENT - RDHI NEXT AGENCY                 
         LA    RE,1(RE)                                                         
         STC   RE,KEY+18                                                        
         B     RDKEYAC                                                          
         SPACE 1                                                                
NXTAC50  XC    KEY+15(12),KEY+15                                                
         CLC   RCONRAGY,TBLAGY     AGENCY FIELD                                 
         BNL   NXTAC48                                                          
         MVC   KEY+15(4),TBLAGY                                                 
         B     RDKEYAC                                                          
         SPACE 1                                                                
NXTAC48  CLC   TBLSTA,ALL          DOES STATION=ALL                             
         BNE   NXTAC40                                                          
         IC    RE,KEY+14           INCREMENT - RDHI NEXT AGY                    
         LA    RE,1(RE)                                                         
         STC   RE,KEY+14                                                        
         B     RDKEYAC                                                          
         SPACE 1                                                                
NXTAC40  XC    KEY+10(17),KEY+10                                                
         CLC   RCONRSTA,TBLSTA     STATION FIELD                                
         BNL   NXTAC38                                                          
         MVC   KEY+10(5),TBLSTA                                                 
         B     RDKEYAC                                                          
         SPACE 1                                                                
NXTAC38  EQU   *                                                                
         CLC   TBLSLS,ALL          DOES SALESPERSON=ALL                         
         BNE   NXTAC30                                                          
         IC    RE,KEY+9            INCREMENT - RDHI NEXT SLS                    
         LA    RE,1(RE)                                                         
         STC   RE,KEY+9                                                         
         B     RDKEYAC                                                          
*                                                                               
NXTAC30  XC    KEY+7(20),KEY+7     SALESPERSON FIELD                            
         CLC   RCONRSAL,TBLSLS                                                  
         BNL   NXTAC28                                                          
         MVC   KEY+7(3),TBLSLS                                                  
         B     RDKEYAC                                                          
*                                                                               
NXTAC28  CLC   TBLTEAM,ALL         DOES TEAM=ALL                                
         BNE   NXTAC20             NO                                           
         IC    RE,KEY+6            YES - BUMP TEAM UP BY 1                      
         LA    RE,1(RE)                                                         
         STC   RE,KEY+6            REPLACE AND READ NEXT                        
         B     RDKEYAC                                                          
*                                                                               
NXTAC20  XC    KEY+6(21),KEY+6     TEAM FIELD                                   
         CLC   RCONRTEM+1(1),TBLTEAM                                            
         BNL   NXTAC18                                                          
         MVC   KEY+6(1),TBLTEAM                                                 
         B     RDKEYAC                                                          
*                                                                               
NXTAC18  CLC   TBLDIV,ALL          DOES DIVISION = ALL                          
         BNE   NXTAC10                                                          
         IC    RE,KEY+5                                                         
         LA    RE,1(RE)                                                         
         STC   RE,KEY+5                                                         
         B     RDKEYAC                                                          
*                                                                               
NXTAC10  XC    KEY+5(22),KEY+5     DIVISION FIELD                               
         CLC   RCONRTEM(1),TBLDIV                                               
         BNL   NXTAC8                                                           
         MVC   KEY+5(1),TBLDIV                                                  
         B     RDKEYAC                                                          
*                                                                               
NXTAC8   IC    RE,KEY+4            OFFICE = ALL AT THIS POINT                   
         LA    RE,1(RE)                                                         
         STC   RE,KEY+4                                                         
         B     RDKEYAC                                                          
         EJECT                                                                  
SETKEY9C XC    KEY,KEY             BUILD KEY                                    
         MVI   KEY,X'9C'           CODE                                         
         MVC   KEY+2(2),REPALPHA   REP                                          
         LA    R8,3                MINIMUM COMPARE LENGTH                       
*                                                                               
         CLC   TBLOFF,ALL          ONLY WHEN ADV NOT ALL, ALL ELSE ALL          
         BE    RDKEY9C                                                          
         MVC   KEY+4(2),TBLOFF     OFFICE                                       
         LA    R8,5                                                             
*                                                                               
         CLC   TBLGRP,ALL                                                       
         BE    RDKEY9C                                                          
*                                  IF GRP NOT ALL, ADV IS NOT ALL,              
*                                  AGY IS ALL                                   
         MVC   KEY+6(7),TBLGRP                                                  
         MVC   KEY+13(4),TBLADV                                                 
         LA    R8,16                                                            
         SPACE 2                                                                
RDKEY9C  BAS   RE,HIGH                                                          
         EX    R8,KEYCLC                                                        
         BNE   KEYDONE                                                          
         MVC   RCONKEY,KEY                                                      
         B     TST9C                                                            
         SPACE 2                                                                
SQKEY9C  BAS   RE,SEQ                                                           
         EX    R8,KEYCLC                                                        
         BNE   KEYDONE                                                          
         MVC   RCONKEY,KEY                                                      
         EJECT                                                                  
TST9C    CLC   TBLGRP,ALL          TEST GROUP                                   
         BE    TST9C10                                                          
         CLC   TBLGRP,RCONQGRP                                                  
         BNE   NXT9C10                                                          
*                                                                               
TST9C10  CLC   TBLSBGP,ALL         TEST SUBGROUP                                
         BE    TST9C20                                                          
         CLC   TBLSBGP,RCONQGRP+1                                               
         BNE   NXT9C20                                                          
*                                                                               
TST9C20  CLC   TBLSTA,ALL          TEST STATION                                 
         BE    TST9C30                                                          
         CLC   TBLSTA,RCONQSTA                                                  
         BNE   NXT9C30                                                          
*                                                                               
TST9C30  CLC   TBLADV,ALL          TEST ADVERTISER                              
         BE    TST9C40                                                          
         CLC   TBLADV,RCONQADV                                                  
         BNE   NXT9C40                                                          
*                                                                               
TST9C40  CLC   TBLAGY,ALL          TEST AGENCY                                  
         BE    TST9C50                                                          
         CLC   TBLAGY,RCONQAGY                                                  
         BNE   NXT9C50                                                          
*                                                                               
TST9C50  CLC   TBLAOFF,ALL                                                      
         BE    TST9C60                                                          
         CLC   TBLAOFF,RCONQAOF                                                 
         BNE   NXT9C60                                                          
*                                                                               
TST9C60  OI    DMINBTS,X'08'                                                    
         BAS   RE,GETREC                                                        
         NI    DMINBTS,X'F7'                                                    
         TM    RCONCNTL,X'80'                                                   
         BO    SQKEY9C                                                          
         BAS   RE,FILTERS                                                       
         BNE   SQKEY9C                                                          
         SPACE 1                                                                
         CLC   TBLFILT,ALL                                                      
         BE    TST9C61                                                          
         CLC   TBLFILT,RCONTYPE                                                 
         BE    TST9C61                                                          
         TM    TBLFILT,X'40'                                                    
         BO    SQKEY9C                                                          
         NI    RCONTYPE,X'BF'                                                   
         CLC   TBLFILT,RCONTYPE                                                 
         BE    SQKEY9C                                                          
         SPACE 1                                                                
TST9C61  MVC   DMCB+4(4),LINK                                                   
         ST    R8,SAVER8                                                        
         B     EXXMOD                                                           
         EJECT                                                                  
NXT9C60  XC    KEY+21(6),KEY+21    AGENCY OFFICE                                
         CLC   RCONQAOF,TBLAOFF                                                 
         BNL   NXT9C58                                                          
         MVC   KEY+21(2),TBLAOFF                                                
         B     RDKEY9C                                                          
*                                                                               
NXT9C58  CLC   TBLAGY,ALL          DOES AGENCY = ALL                            
         BNE   NXT9C50                                                          
         IC    RE,KEY+20           INCREMENT - RDHI NEXT AGY                    
         LA    RE,1(RE)                                                         
         STC   RE,KEY+20                                                        
         B     RDKEY9C                                                          
*                                                                               
NXT9C50  XC    KEY+17(10),KEY+17   AGENCY                                       
         CLC   RCONQAGY,TBLAGY                                                  
         BNL   NXT9C48                                                          
         MVC   KEY+17(4),TBLAGY                                                 
         B     RDKEY9C                                                          
         SPACE 1                                                                
NXT9C48  CLC   TBLADV,ALL          ADVERTISER ALL OPTION                        
         BNE   NXT9C40                                                          
         IC    RE,KEY+16           INCREMENT - RDHI NEXT ADVERTISER             
         LA    RE,1(RE)                                                         
         STC   RE,KEY+16                                                        
         B     RDKEY9C                                                          
         SPACE 1                                                                
NXT9C40  XC    KEY+13(14),KEY+13   ADVERTISER SPECIFIC                          
         CLC   RCONQADV,TBLADV                                                  
         BNL   NXT9C38                                                          
         MVC   KEY+13(4),TBLADV                                                 
         B     RDKEY9C                                                          
         SPACE 1                                                                
NXT9C38  CLC   TBLSTA,ALL          STATION ALL OPTION                           
         BNE   NXT9C30                                                          
         IC    RE,KEY+12                                                        
         LA    RE,1(RE)                                                         
         STC   RE,KEY+12                                                        
         B     RDKEY9C                                                          
         SPACE 1                                                                
NXT9C30  XC    KEY+8(19),KEY+8     STATION SPECIFIC                             
         CLC   RCONQSTA,TBLSTA                                                  
         BNL   NXT9C28                                                          
         MVC   KEY+8(5),TBLSTA                                                  
         B     RDKEY9C                                                          
         SPACE 1                                                                
NXT9C28  CLC   TBLSBGP,ALL                                                      
         BNE   NXT9C20                                                          
         IC    RE,KEY+7                                                         
         LA    RE,1(RE)                                                         
         STC   RE,KEY+7                                                         
         B     RDKEY9C                                                          
         SPACE 1                                                                
NXT9C20  XC    KEY+7(20),KEY+7                                                  
         CLC   RCONQGRP+1(1),TBLSBGP                                            
         BNL   NXT9C18                                                          
         MVC   KEY+7(1),TBLSBGP                                                 
         B     RDKEY9C                                                          
         SPACE 1                                                                
NXT9C18  CLC   TBLGRP,ALL                                                       
         BNE   NXT9C10                                                          
         IC    RE,KEY+6                                                         
         LA    RE,1(RE)                                                         
         STC   RE,KEY+6                                                         
         B     RDKEY9C                                                          
         SPACE 1                                                                
NXT9C10  XC    KEY+6(21),KEY+6                                                  
         CLC   RCONQGRP(1),TBLGRP                                               
         BNL   NXT9C08                                                          
         MVC   RCONQGRP(1),TBLGRP                                               
         B     RDKEY9C                                                          
         SPACE 1                                                                
NXT9C08  IC    RE,KEY+5                                                         
         LA    RE,1(RE)                                                         
         STC   RE,KEY+5                                                         
         B     RDKEY9C                                                          
         EJECT                                                                  
SETKEY0C XC    KEY,KEY             BUILD KEY                                    
         MVI   KEY,X'0C'           CODE                                         
         MVC   KEY+2(2),REPALPHA   REP                                          
         LA    R8,3                MINIMUM COMPARE LENGTH                       
*                                                                               
         CLC   TBLGRP,ALL                                                       
         BE    RDKEY0C                                                          
         MVC   KEY+4(1),TBLGRP                                                  
         LA    R8,4                                                             
*                                                                               
         CLC   TBLSBGP,ALL                                                      
         BE    RDKEY0C                                                          
         MVC   KEY+5(1),TBLSBGP                                                 
         LA    R8,5                                                             
*                                                                               
         CLC   TBLSTA,ALL                                                       
         BE    RDKEY0C                                                          
         MVC   KEY+6(5),TBLSTA                                                  
         LA    R8,10                                                            
*                                                                               
         CLC   TBLOFF,ALL                                                       
         BE    RDKEY0C                                                          
         MVC   KEY+11(2),TBLOFF                                                 
*                                                                               
         LA    R8,12                                                            
*                                                                               
         CLC   TBLAGY,ALL          AGENCY                                       
         BE    RDKEY0C                                                          
         MVC   KEY+13(4),TBLAGY                                                 
         LA    R8,16                                                            
*                                                                               
         CLC   TBLAOFF,ALL                                                      
         BE    RDKEY0C                                                          
         MVC   KEY+17(2),TBLAOFF                                                
         LA    R8,18                                                            
*                                                                               
         CLC   TBLADV,ALL                                                       
         BE    RDKEY0C                                                          
         MVC   KEY+19(4),TBLADV                                                 
         LA    R8,22                                                            
         SPACE 2                                                                
RDKEY0C  BAS   RE,HIGH                                                          
         EX    R8,KEYCLC                                                        
         BNE   KEYDONE                                                          
         MVC   RCONKEY,KEY                                                      
         B     TST0C                                                            
         SPACE 2                                                                
SQKEY0C  BAS   RE,SEQ                                                           
         EX    R8,KEYCLC                                                        
         BNE   KEYDONE                                                          
         MVC   RCONKEY,KEY                                                      
         EJECT                                                                  
*                                                                               
TST0C    CLC   TBLOFF,ALL                                                       
         BE    TST0C20                                                          
         CLC   RCONKOFF,TBLOFF                                                  
         BNE   NXT0C20                                                          
*                                                                               
TST0C20  CLC   TBLAGY,ALL          TEST AGENCY                                  
         BE    TST0C30                                                          
         CLC   RCONKAGY,TBLAGY                                                  
         BNE   NXT0C30                                                          
*                                                                               
TST0C30  CLC   TBLAOFF,ALL         TEST AGENCY OFFICE                           
         BE    TST0C40                                                          
         CLC   TBLAOFF,RCONKAOF                                                 
         BNE   NXT0C40                                                          
TST0C40  CLC   TBLADV,ALL          TEST ADVERTISER                              
         BE    TST0C50                                                          
         CLC   RCONKADV,TBLADV                                                  
         BNE   NXT0C50                                                          
*                                                                               
TST0C50  BAS   RE,GETREC                                                        
         BAS   RE,FILTERS                                                       
         BNE   SQKEY0C                                                          
         CLC   TBLFILT,ALL                                                      
         BE    TST0C51                                                          
         CLC   TBLFILT,RCONTYPE                                                 
         BE    TST0C51                                                          
         TM    TBLFILT,X'40'                                                    
         BO    SQKEY0C                                                          
         NI    RCONTYPE,X'BF'                                                   
         CLC   TBLFILT,RCONTYPE                                                 
         BE    SQKEY0C                                                          
         SPACE 1                                                                
TST0C51  MVC   DMCB+4(4),LINK                                                   
         ST    R8,SAVER8                                                        
         B     EXXMOD                                                           
         EJECT                                                                  
NXT0C50  XC    KEY+19(8),KEY+19    ADVERTISER                                   
         CLC   RCONKADV,TBLADV     LAST FIELD CAN'T BE ALL AT THIS PT           
         BNL   NXT0C48                                                          
         MVC   KEY+19(4),TBLADV                                                 
         B     RDKEY0C                                                          
         SPACE 1                                                                
NXT0C48  CLC   TBLAOFF,ALL         AGENCY OFFICE ALL OPTION                     
         BNE   NXT0C40                                                          
         IC    RE,KEY+18                                                        
         LA    RE,1(RE)                                                         
         STC   RE,KEY+18                                                        
         B     RDKEY0C                                                          
         SPACE 1                                                                
NXT0C40  XC    KEY+17(10),KEY+17   AGENCY OFFICE SPECIFIC                       
         CLC   RCONKAOF,TBLAOFF                                                 
         BNL   NXT0C38                                                          
         MVC   KEY+17(2),TBLAOFF                                                
         B     RDKEY0C                                                          
         SPACE 1                                                                
NXT0C38  CLC   TBLAGY,ALL          AGENCY ALL OPTION                            
         BNE   NXT0C30                                                          
         IC    RE,KEY+16                                                        
         LA    RE,1(RE)                                                         
         STC   RE,KEY+16                                                        
         B     RDKEY0C                                                          
         SPACE 1                                                                
NXT0C30  XC    KEY+13(14),KEY+13   AGENCY SPECIFIC                              
         CLC   RCONKAGY,TBLAGY                                                  
         BNL   NXT0C28                                                          
         MVC   KEY+13(4),TBLAGY                                                 
         B     RDKEY0C                                                          
         SPACE 1                                                                
NXT0C28  CLC   TBLOFF,ALL          OFFICE ALL                                   
         BNE   NXT0C20                                                          
         IC    RE,KEY+12                                                        
         LA    RE,1(RE)                                                         
         STC   RE,KEY+12                                                        
         B     RDKEY0C                                                          
         SPACE 1                                                                
NXT0C20  XC    KEY+11(16),KEY+11   OFFICE SPECIFIC                              
         CLC   RCONKOFF,TBLOFF                                                  
         BNL   NXT0C18                                                          
         MVC   KEY+11(2),TBLOFF                                                 
         B     RDKEY0C                                                          
         SPACE 1                                                                
NXT0C18  IC    RE,KEY+10           STATION MUST BE ALL                          
         LA    RE,1(RE)                                                         
         STC   RE,KEY+10                                                        
         B     RDKEY0C                                                          
         EJECT                                                                  
SETKEYBC XC    KEY,KEY                                                          
         MVI   KEY,X'BC'           BUILD UNITL FIRST ALL                        
         MVC   KEY+2(2),REPALPHA                                                
         MVC   KEY+4(2),TBLCTG                                                  
         LA    R8,5                SET MANDATORY COMPARE LENGTH-1               
*                                                                               
         CLC   TBLOFF,ALL          TEST OFFICE                                  
         BE    RDKEYBC                                                          
         MVC   KEY+6(2),TBLOFF                                                  
         LA    R8,7                                                             
*                                                                               
         CLC   TBLSTA,ALL          TEST STATION                                 
         BE    RDKEYBC                                                          
         MVC   KEY+8(5),TBLSTA                                                  
         LA    R8,12                                                            
*                                                                               
         CLC   TBLAGY,ALL                                                       
         BE    RDKEYBC                                                          
         MVC   KEY+13(4),TBLAGY                                                 
         LA    R8,16                                                            
*                                                                               
         CLC   TBLAOFF,ALL                                                      
         BE    RDKEYBC                                                          
         MVC   KEY+17(2),TBLAOFF                                                
         LA    R8,18                                                            
*                                                                               
         CLC   TBLADV,ALL                                                       
         BE    RDKEYBC                                                          
         MVC   KEY+19(4),TBLADV                                                 
         LA    R8,22                                                            
         SPACE 2                                                                
RDKEYBC  BAS   RE,HIGH                                                          
         EX    R8,KEYCLC           TEST LENGTH                                  
         BNE   KEYDONE                                                          
         MVC   RCONKEY,KEY                                                      
         B     TSTBC                                                            
         SPACE 2                                                                
SQKEYBC  BAS   RE,SEQ                                                           
         EX    R8,KEYCLC                                                        
         BNE   KEYDONE                                                          
         MVC   RCONKEY,KEY                                                      
         B     TSTBC                                                            
         EJECT                                                                  
TSTBC    CLC   TBLSTA,ALL                                                       
         BE    TSTBC10                                                          
         CLC   TBLSTA,RCONDSTA                                                  
         BNE   NXTBC10                                                          
*                                                                               
TSTBC10  CLC   TBLAGY,ALL                                                       
         BE    TSTBC20                                                          
         CLC   TBLAGY,RCONDAGY                                                  
         BNE   NXTBC20                                                          
*                                                                               
TSTBC20  CLC   TBLAOFF,ALL                                                      
         BE    TSTBC30                                                          
         CLC   TBLAOFF,RCONDAOF                                                 
         BNE   NXTBC30                                                          
*                                                                               
TSTBC30  CLC   TBLADV,ALL                                                       
         BE    TSTBC40                                                          
         CLC   TBLADV,RCONDADV                                                  
         BNE   NXTBC40                                                          
TSTBC40  OI    DMINBTS,X'08'                                                    
         BAS   RE,GETREC                                                        
         NI    DMINBTS,X'F7'                                                    
         TM    RCONCNTL,X'80'                                                   
         BO    SQKEYBC                                                          
         BAS   RE,FILTERS                                                       
         BNE   SQKEYBC                                                          
*                                                                               
         CLC   TBLGRP,ALL                                                       
         BE    TSTBC42                                                          
         CLC   TBLGRP,RCONKGRP                                                  
         BNE   SQKEYBC                                                          
*                                                                               
TSTBC42  CLC   TBLSBGP,ALL                                                      
         BE    TSTBC44                                                          
         CLC   TBLSBGP,RCONKGRP+1                                               
         BNE   SQKEYBC                                                          
*                                                                               
TSTBC44  CLC   TBLFILT,ALL                                                      
         BE    TSTBC45                                                          
         CLC   TBLFILT,RCONTYPE                                                 
         BE    TSTBC45                                                          
         TM    TBLFILT,X'40'                                                    
         BO    SQKEYBC                                                          
         NI    RCONTYPE,X'BF'                                                   
         CLC   TBLFILT,RCONTYPE                                                 
         BE    SQKEYBC                                                          
         SPACE 1                                                                
TSTBC45  EQU   *                                                                
         MVC   DMCB+4(4),LINK                                                   
         ST    R8,SAVER8                                                        
         B     EXXMOD                                                           
         EJECT                                                                  
* CATEGORY MUST BE PART OF MANDATORY KEY FOR BC KEYS. OFFICE IS EITHER          
* PART OF KEY OR ALL, SO NEITHER HAS TO BE TESTED.                              
*                                                                               
NXTBC40  XC    KEY+19(8),KEY+19    ADVERTISER                                   
         CLC   RCONDADV,TBLADV                                                  
         BNL   NXTBC38                                                          
         MVC   KEY+19(4),TBLADV                                                 
         B     RDKEYBC                                                          
         SPACE 1                                                                
NXTBC38  CLC   TBLAOFF,ALL          DOES AGENCY OFFICE=ALL                      
         BNE   NXTBC30                                                          
         IC    RE,KEY+18           YES- INCREMENT AND RDHI                      
         LA    RE,1(RE)                                                         
         STC   RE,KEY+18                                                        
         B     RDKEYBC                                                          
         SPACE 1                                                                
NXTBC30  XC    KEY+17(10),KEY+17   AGENCY OFFICE                                
         CLC   RCONDAOF,TBLAOFF                                                 
         BNL   NXTBC28                                                          
         MVC   KEY+17(2),TBLAOFF                                                
         B     RDKEYBC                                                          
         SPACE 1                                                                
NXTBC28  CLC   TBLAGY,ALL          DOES AGENCY=ALL                              
         BNE   NXTBC20                                                          
         IC    RE,KEY+16           YES-INCREMENT AND RDHI                       
         LA    RE,1(RE)                                                         
         STC   RE,KEY+16                                                        
         B     RDKEYBC                                                          
         SPACE 1                                                                
NXTBC20  XC    KEY+13(14),KEY+13   AGENCY                                       
         CLC   RCONDAGY,TBLAGY                                                  
         BNL   NXTBC18                                                          
         MVC   KEY+13(4),TBLAGY                                                 
         B     RDKEYBC                                                          
         SPACE 1                                                                
NXTBC18  CLC   TBLSTA,ALL          DOES STATION=ALL                             
         BNE   NXTBC10                                                          
         IC    RE,KEY+12           YES-INCREMENT AND RDHI                       
         LA    RE,1(RE)                                                         
         STC   RE,KEY+12                                                        
         B     RDKEYBC                                                          
         SPACE 1                                                                
NXTBC10  XC    KEY+8(19),KEY+8     STATION                                      
         CLC   RCONDSTA,TBLSTA                                                  
         BNL   NXTBC08                                                          
         MVC   KEY+8(5),TBLSTA                                                  
         B     RDKEYBC                                                          
         SPACE 1                                                                
NXTBC08  IC    RE,KEY+7            OFFICE MUST BE ALL SO                        
         LA    RE,1(RE)                                                         
         STC   RE,KEY+7            INCREMENT AND RDHI                           
         B     RDKEYBC                                                          
         EJECT                                                                  
KEYDONE  MVC   DMCB+4(4),LINK                                                   
         MVI   DMCB,X'FF'                                                       
         B     EXXMOD                                                           
         SPACE 3                                                                
*                                                                               
* CONTRACT STATUS ROUTINES                                                      
* PRODUCT FILTER                                                                
FILTERS  NTR1                                                                   
*                                                                               
* ONLY PASS RECORDS WITH SHR PCT                                                
         TM    VIEWS2,X'10'+X'20'  SHR PCT FILTERING?                           
         BZ    FIL10               NO                                           
*                                                                               
         L     R4,AIOAREA                                                       
         LA    R4,RCONELEM-RCONREC(R4)                                          
FIL02    ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FIL04               NO SHR PCT ON CONTRACT                       
         CLI   0(R4),X'06'                                                      
         BNE   FIL02                                                            
*                                                                               
         TM    RCONSPES-RCONSPEL(R4),X'04'                                      
         BZ    FIL04               NO SHR PCT ON CONTRACT                       
*                                                                               
         TM    VIEWS2,X'20'        EXCLUDE CONTRACTS WITH SHR PCT?              
         BO    FLTNO               YES/SO SKIP THIS CONTRACT                    
         B     FIL10                                                            
*                                                                               
FIL04    DS    0H                                                               
         TM    VIEWS2,X'10'       ONLY INCLUDE CONTRACTS WITH SHR PCT?          
         BO    FLTNO              YES/SO SKIP CONTRACT                          
*                                                                               
FIL10    TM    VIEWS,X'20'+X'40'   SENT BY FILTER ?                             
         BZ    FIL20               NO                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,WORK)     GET TODAY'S DATE                 
*                                                                               
         TM    VIEWS,X'20'         SENT BY STATION TODAY FILTER ?               
         BZ    FIL14                                                            
*                                                                               
         BAS   RE,GETSEND          POINTS R4 AT SEND ELEMENT                    
         USING RCONSEND,R4                                                      
         CLC   RCONSSDT,WORK       SENT BY STATION TODAY ?                      
         BE    FIL14               YES                                          
*                                                                               
         BAS   RE,GETSEND2                                                      
         USING RCONMGEL,R4                                                      
         TM    RCONMGFG,X'20'      LAST SENT BY STA ?                           
         BNO   FLTNO                                                            
         CLC   RCONMGDT,WORK       SENT BY STA TODAY?                           
         BNE   FLTNO                                                            
                                                                                
FIL14    TM    VIEWS,X'40'         SENT BY REP TODAY FILTER ?                   
         BZ    FIL20                                                            
*                                                                               
         BAS   RE,GETSEND          POINTS R4 AT SEND ELEMENT                    
         USING RCONSEND,R4                                                      
         CLC   RCONSRDT,WORK       SENT BY REP TODAY ?                          
         BE    FIL20               YES                                          
*                                                                               
         BAS   RE,GETSEND2         MKGD OFFER SENT BY REP TODAY?                
         USING RCONMGEL,R4                                                      
         TM    RCONMGFG,X'40'      LAST SENT BY REP ?                           
         BNO   FLTNO                                                            
*                                                                               
         CLI   RCONMGLN,RCONMGLQ   OLD ELEM ?                                   
         BNE   FIL16                                                            
         CLC   RCONMGDT,WORK       SENT BY REP TODAY?                           
         BNE   FLTNO               NO                                           
         B     FIL20                                                            
*                                                                               
FIL16    CLI   RCONMGLN,RCONXMGQ   NEW ELEM                                     
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
         CLC   RCONRMDT,WORK                                                    
         BNE   FLTNO                                                            
         DROP  R4                                                               
*                                                                               
FIL20    EQU   *                                                                
         TM    VIEWS2,X'04'+X'08'  DARE FILTERING?                              
         BZ    FIL30               NO                                           
*                                                                               
         L     R4,AIOAREA                                                       
         LA    R4,RCONELEM-RCONREC(R4)                                          
FIL22    DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FIL24               CONTRACT IS NOT DARE                         
         CLI   0(R4),X'1D'                                                      
         BNE   FIL22                                                            
*                                                                               
         TM   RCONDRFG-RCONDREL(R4),X'04'+X'02'                                 
         BNZ  FIL24               CONTRACT IS NOT DARE                          
*                                                                               
         TM   VIEWS2,X'08'         EXCLUDE DARE FILTER?                         
         BO   FLTNO                YES - CONTRACT IS DARE SO SKIP               
         B    FIL30                                                             
*                                                                               
FIL24    DS   0H                                                                
         TM   VIEWS2,X'04'         INCLUDE DARE FILTER?                         
         BO   FLTNO                YES - SKIP                                   
*                                                                               
FIL30    EQU   *                                                                
         CLI   DVSFILT,C' '        DEVSAL FITLER?                               
         BNH   FIL40               NO                                           
*                                                                               
         L     R4,AIOAREA                                                       
         LA    R4,RCONELEM-RCONREC(R4)                                          
FIL32    ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FLTNO               NO MATCH                                     
         CLI   0(R4),X'18'                                                      
         BNE   FIL32                                                            
*                                                                               
         CLC   DVSFILT,RCONDVSP-RCONDVEL(R4)                                    
         BNE   FLTNO               NO MATCH                                     
*                                                                               
FIL40    EQU   *                                                                
         CLI   DVTFILT,C' '        DEVTYPE FITLER?                              
         BNH   FIL50               NO                                           
*                                                                               
         L     R4,AIOAREA                                                       
         LA    R4,RCONELEM-RCONREC(R4)                                          
FIL42    ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FLTNO               NO MATCH                                     
         CLI   0(R4),X'18'                                                      
         BNE   FIL42                                                            
*                                                                               
         CLC   DVTFILT,RCONDVCT-RCONDVEL(R4)                                    
         BNE   FLTNO               NO MATCH                                     
*                                                                               
FIL50    EQU   *                                                                
         TM    VIEWS2,X'01'+X'02'  PENDING FILTERING?                           
         BZ    FIL60               NO                                           
*                                                                               
         L     R4,AIOAREA                                                       
         LA    R4,RCONELEM-RCONREC(R4)                                          
         USING RCONELEM,R4                                                      
         CLI   0(R4),X'01'         DESCRIPTION ELEMENT REQ'D                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RCONMODR,X'10'      BUY LINE ADDED?                              
         BO    FIL59NP             YES - NOT PENDING                            
         DROP  R4                                                               
*                                                                               
FIL51    DS    0H                  BUCKETS?                                     
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FIL52               NO                                           
         CLI   0(R4),X'03'                                                      
         BE    FIL59NP             YES - NOT PENDING                            
         B     FIL51                                                            
*                                                                               
FIL52    DS    0H                                                               
         L     R4,AIOAREA                                                       
         LA    R4,RCONELEM-RCONREC(R4)                                          
FIL52A   DS    0H                  STATION CONTRACT AMOUNT?                     
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FIL53               NO                                           
         CLI   0(R4),X'04'                                                      
         BE    FIL59NP             YES - NOT PENDING                            
         B     FIL52A                                                           
*                                                                               
FIL53    DS    0H                                                               
         L     R4,AIOAREA                                                       
         LA    R4,RCONELEM-RCONREC(R4)                                          
FIL53A   DS    0H                  SPL/EPL?                                     
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FIL54               NO                                           
         CLI   0(R4),X'06'                                                      
         BE    FIL59NP             YES - NOT PENDING                            
         B     FIL53A                                                           
*                                                                               
FIL54    DS    0H                                                               
         L     R4,AIOAREA                                                       
         LA    R4,RCONELEM-RCONREC(R4)                                          
FIL54A   DS    0H                  SPL COMMENT?                                 
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FIL55               NO                                           
         CLI   0(R4),X'07'                                                      
         BE    FIL59NP             YES - NOT PENDING                            
         B     FIL54A                                                           
*                                                                               
FIL55    DS    0H                                                               
         L     R4,AIOAREA                                                       
         LA    R4,RCONELEM-RCONREC(R4)                                          
FIL55A   DS    0H                  PENDING ELEMENT?                             
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FIL56               NO - CALL IT PENDING                         
         CLI   0(R4),X'12'                                                      
         BNE   FIL55A                                                           
*                                                                               
         USING RSARXEL,R4                                                       
         TM    RSARXFLG,X'18'      FORCAST?                                     
         BNZ   FIL59NP             YES - NOT PENDING                            
         DROP  R4                                                               
*                                                                               
FIL56    DS    0H                                                               
*                                                                               
FIL59    DS    0H                  RECORD IS PENDING                            
         TM    VIEWS2,X'02'        EXCLUDE PENDING FILTER?                      
         BO    FLTNO               YES - SO SKIP                                
         B     FIL60                                                            
*                                                                               
FIL59NP  DS    0H                  RECORD IS NOT PENDING                        
         TM    VIEWS2,X'01'        INCLUDE PENDING FILTER?                      
         BO    FLTNO               YES - SKIP                                   
*                                                                               
FIL60    EQU   *                                                                
         TM    VIEWS2,X'40'+X'80'  FORECAST FILTERING?                          
         BZ    FIL70               NO                                           
*                                                                               
         L     R4,AIOAREA                                                       
         LA    R4,RCONELEM-RCONREC(R4)                                          
FIL62    DS    0H                  PENDING ELEMENT?                             
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FIL64               NO - NOT FORECAST                            
         CLI   0(R4),X'12'                                                      
         BNE   FIL62                                                            
*                                                                               
         USING RSARXEL,R4                                                       
         TM    RSARXFLG,X'18'      FORCAST?                                     
         BZ    FIL64               NO                                           
*                                                                               
         TM   VIEWS2,X'80'         EXCLUDE FORECAST FILTER?                     
         BO   FLTNO                YES - SO SKIP                                
         B    FIL70                                                             
*                                                                               
FIL64    DS    0H                  RECORD IS NOT FORECAST                       
         TM   VIEWS2,X'40'         INCLUDE FORECAST FILTER?                     
         BO   FLTNO                YES - SKIP                                   
*******************                                                             
** SET FILTERING **                                                             
*******************                                                             
FIL70    DS    0H                                                               
         LR    RE,RA               FIRST  SET                                   
         AH    RE,=Y(FIRSTSET)                                                  
FIL71    DS    0H                                                               
         CLI   0(RE),0             ANY SET HERE?                                
         BE    FIL80               NO                                           
*                                                                               
         L     R4,AIOAREA          YES                                          
         USING RCONREC,R4                                                       
*                                                                               
         CLC   =C'AG',1(RE)        AGENCY?                                      
         BNE   *+12                NO                                           
         LA    R4,RCONKAGY                                                      
         B     FIL72                                                            
*                                                                               
         CLC   =C'AD',1(RE)        ADVERTISER?                                  
         BNE   *+12                NO                                           
         LA    R4,RCONKADV                                                      
         B     FIL72                                                            
*                                                                               
         CLC   =C'SP',1(RE)        SALESPERSON?                                 
         BNE   *+12                NO                                           
         LA    R4,RCONSAL                                                       
         B     FIL72                                                            
*                                                                               
         CLC   =C'CT',1(RE)        CONTRACT TYPE?                               
         BNE   *+12                NO                                           
         LA    R4,RCONTYPE                                                      
         B     FIL72                                                            
*                                                                               
         CLC   =C'OF',1(RE)        OFFICE?                                      
         BNE   *+12                NO                                           
         LA    R4,RCONKOFF                                                      
         B     FIL72                                                            
*                                                                               
         CLC   =C'ST',1(RE)        STATION?                                     
         BNE   *+12                NO                                           
         LA    R4,RCONKSTA                                                      
         B     FIL72                                                            
*                                                                               
         CLC   =C'GS',1(RE)        GROUP/SUP-GROUP?                             
         BNE   *+12                NO                                           
         LA    R4,RCONKGRP                                                      
         B     FIL72                                                            
*                                                                               
         CLC   =C'DT',1(RE)        DEVELOPMENTAL TYPE?                          
         BE    *+6                 YES                                          
         DC    H'0'                NO - TWA WACKED                              
*                                                                               
         LA    R4,RCONELEM                                                      
         USING RCONDVEL,R4                                                      
FIL71Z   DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             EOR?                                         
         BE    FLTNO               YES                                          
         CLI   0(R4),X'18'         DEVELOPMENTAL INFO ELEMENT?                  
         BNE   FIL71Z              NO                                           
*                                                                               
         LA    R4,RCONDVCT                                                      
         DROP  R4                                                               
*                                                                               
FIL72    DS    0H                                                               
         MVC   BYTE,0(RE)          SAVE FLAGS IN BYTE                           
         ZIC   RF,3(RE)            SAVE LENGTH TO COMPARE                       
         BCTR  RF,0                                                             
         LA    RE,4(RE)            SKIP TABLE HEADER                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),0(R4)       VALUE SET?                                   
         BZ    FLTNO               NO - ALWAYS SKIP ON SET FILTER               
*                                                                               
FIL74    DS    0H                                                               
         CLI   0(RE),0                                                          
         BE    FIL76               MATCH NOT FOUND                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(R4)                                                    
         BE    *+12                                                             
         LA    RE,1(RF,RE)                                                      
         B     FIL74                                                            
*                                                                               
         TM    BYTE,X'40'          EXCLUSION SET?                               
         BO    FLTNO               YES - MATCH FOUND SO SKIP                    
         B     FIL78                                                            
*                                                                               
FIL76    TM    BYTE,X'40'          EXCLUSION SET?                               
         BNO   FLTNO               NO - MATCH NOT FOUND SKIP                    
*                                                                               
FIL78    CLI   0(RE),0                                                          
         BE    *+12                                                             
         LA    RE,1(RF,RE)                                                      
         B     *-12                                                             
*                                                                               
         LA    RE,1(RE)                                                         
         B     FIL71                                                            
*                                                                               
FIL80    DS    0H                                                               
         TM    VIEWS3,X'40'+X'20'  TAKEOVER FILTERING?                          
         BZ    FIL90               NO                                           
*                                                                               
         L     R4,AIOAREA                                                       
         LA    R4,RCONELEM-RCONREC(R4)                                          
FIL82    DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    FIL84               CONTRACT IS NOT TAKEOVER                     
         CLI   0(R4),X'2A'         TKO?                                         
         BE    FIL83               YES                                          
         CLI   0(R4),X'2C'         BACKBILL/ALLHIST?                            
         BNE   FIL82                                                            
*                                                                               
FIL83    EQU   *                                                                
         TM   VIEWS3,X'40'         EXCLUDE TAKEOVER FILTER?                     
         BO   FLTNO                YES - CONTRACT IS TAKEOVER SO SKIP           
         B    FIL90                                                             
*                                                                               
FIL84    DS   0H                                                                
         TM   VIEWS3,X'20'         INCLUDE TAKEOVER FILTER?                     
         BO   FLTNO                YES - SKIP                                   
*                                                                               
FIL90    DS   0H                                                                
*                                                                               
*------------------------------------------------------------------*            
         CLI   PRDFILT,X'40'       ARE WE FILTERING ON PRODUCT?                 
         BNH   FLTBUYDT                                                         
         L     R4,AIOAREA          YES                                          
         USING RCONREC,R4                                                       
         CLC   RCONPRD,PRDFILT                                                  
         BNE   FLTNO                                                            
         B     FLTBUYDT                                                         
         DROP  R4                                                               
*                                                                               
FLTBUYDT CLI   TBLBDATS,0          BUY DATE FILTER?                             
         BE    FLT00                                                            
         L     R4,AIOAREA          YES                                          
         USING RCONREC,R4                                                       
         CLC   RCONCREA,TBLBDATS   BUY DATE VS BUY DATE START FILTER            
         BL    FLTNO                                                            
         CLC   RCONCREA,TBLBDATE   BUY DATE VS BUY DATE END FILTER              
         BH    FLTNO                                                            
         B     FLT00                                                            
         DROP  R4                                                               
                                                                                
*                                                                               
FLT00    CLI   FILTFLG,0           FILTFLG FILTERS?                             
         BE    FLTTBLT                                                          
         L     R4,AIOAREA                                                       
         USING RCONREC,R4                                                       
         LA    R4,RCONELEM                                                      
FLT000   ZIC   RE,1(R4)                                                         
         CLI   1(R4),0                                                          
         BE    FLT001                                                           
         AR    R4,RE                                                            
         CLI   0(R4),X'17'         COMBO?                                       
         BNE   FLT000                                                           
         DROP  R4                                                               
         TM    FILTFLG,X'02'        YES - EXCLUDE COMBOS ?                      
         BO    FLTNO                      YES SO EXCLUDE THIS CONTRACT          
         B     FLT005                     NO                                    
*                                                                               
FLT001   TM    FILTFLG,X'01'         NO - COBOS ONLY?                           
         BO    FLTNO                    YES SO SKIP THIS CONTRACT               
         B     FLT005                     NO                                    
*                                                                               
FLT005   EQU   *                                                                
*                                                                               
FLTTBLT  CLI   TBLCTYP,0           CONTRACT STATUS FILTER ?                     
         BE    FLTOK               NO                                           
         L     R4,AIOAREA          YES                                          
         USING RCONREC,R4                                                       
         CLI   TBLCTYP,C'M'        OUTSTANDING MAKEGOOD OFFERS?                 
         BNE   FLT02                                                            
         LA    R4,RCONELEM                                                      
FLT01    ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    FLTNO                                                            
         AR    R4,R1                                                            
         CLI   0(R4),X'21'                                                      
         BNE   FLT01                                                            
         LR    R1,R4                                                            
         USING RCONMGEL,R1                                                      
         TM    RCONMGCT,X'80'      MKGD OFFERS EXIST?                           
         BO    FLTOK                                                            
         B     FLTNO                                                            
         DROP  R1                                                               
                                                                                
         DROP  R4                                                               
FLT02    L     R4,AIOAREA                                                       
         USING RCONREC,R4                                                       
         CLC   =C'ACC-',RCONBUYR    SPECIAL CONFIRMED CONTRACT ?                
         BNE   FLT03                                                            
         CLI   TBLCTYP,C'C'         YES/ ONLY MATCH WITH CONFIRMED              
         BNE   FLTNO                                                            
         CLC   =C'CO',TBLCTYP           BUT NOT WITH 'CO'                       
         BE    FLTNO                                                            
         B     FLTOK                                                            
                                                                                
         DROP  R4                                                               
FLT03    L     R4,AIOAREA                                                       
         USING RCONREC,R4                                                       
         TM    RCONMODR+1,X'80'    'ACE' ORDER?                                 
         BO    FLT04               YES                                          
         TM    RCONMODR+1,X'40'    'GRAPHNET' ORDER?                            
         BO    FLT04               YES                                          
         CLI   TBLCTYP,C'C'         YES/ ONLY MATCH WITH CONFIRMED              
         BNE   FLTNO                                                            
         B     FLTOK                                                            
                                                                                
         DROP  R4                                                               
FLT04    MVI   BYTE,0                                                           
         L     R4,AIOAREA                                                       
         USING RCONREC,R4                                                       
         LA    R4,RCONELEM                                                      
FLT05    ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    FLTOK                                                            
         AR    R4,R1                                                            
         CLI   0(R4),X'1F'                                                      
         BNE   FLT05                                                            
         LR    R1,R4                                                            
         USING RCONXEL,R1                                                       
         MVC   BYTE,RCONCONF                                                    
         DROP  R1                                                               
*                                                                               
         CLC   TBLCTYP,=C'U$ '      NEVER CONFIRMED                             
         BNE   FLT07                                                            
         TM    BYTE,X'60'          CONFIRMED NOW OR PREVIOUSLY ?                
         BZ    FLTOK               NO - OK                                      
         B     FLTNO               YES - REJECT                                 
*                                                                               
FLT07    CLC   TBLCTYP,=C'UO '       NOT CONFIRMED NOW                          
         BNE   FLT09                                                            
         TM    BYTE,X'40'          CONFIRMED NOW ?                              
         BZ    FLTOK               NO - OK                                      
         B     FLTNO               YES - REJECT                                 
*                                                                               
FLT09    CLC   TBLCTYP,=C'C$ '      CONFIRMED AT LEAST ONCE                     
         BNE   FLT11                                                            
         TM    BYTE,X'60'          CONFIRMED NOW OR PREVIOUSLY ?                
         BZ    FLTNO               NO - REJECT                                  
         B     FLTOK                                                            
*                                                                               
FLT11    CLC   TBLCTYP,=C'CO '     CONFIRMED NOW                                
         BNE   FLT13                                                            
         TM    BYTE,X'40'          CONFIRMED NOW ?                              
         BO    FLTOK               YES - OK                                     
         B     FLTNO               NO - REJECT                                  
*                                                                               
         DROP  R4                                                               
FLT13    L     R4,AIOAREA                                                       
         USING RCONREC,R4                                                       
         LA    R4,RCONELEM                                                      
FLT14    ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    FLTNO                                                            
         AR    R4,R1                                                            
         CLI   0(R4),X'20'                                                      
         BNE   FLT14                                                            
         LR    R1,R4                                                            
         USING RCONSEND,R1                                                      
         MVC   DUB(1),RCONSRV       REP VERSION NUMBER                          
         MVC   DUB+1(1),RCONSSV     STATION VERSION NUMBER                      
         MVC   DUB+2(1),RCONSENF    SEND FLAG                                   
         DROP  R1,R4                                                            
*                                                                               
         CLC   =C'USW',TBLCTYP     USW-STATION VERSION                          
         BNE   FLT15                                                            
         TM    BYTE,X'40'          CONFIRMED NOW?                               
         BO    FLTNO               YES-REJECT                                   
         CLC   DUB+1(1),DUB        STATION VERSION HIGHER?                      
         BH    FLTOK                                                            
         B     FLTNO                                                            
*                                                                               
FLT15    CLC   =C'URW',TBLCTYP     URW-REP VERSION                              
         BNE   FLT17                                                            
         TM    BYTE,X'40'          CONFIRMED NOW?                               
         BO    FLTNO               YES-REJECT                                   
         CLC   DUB(1),DUB+1        REP VERSION HIGHER?                          
         BH    FLTOK                                                            
         B     FLTNO                                                            
*                                                                               
FLT17    CLC   =C'US',TBLCTYP      US-STATION - NO WIPS                         
         BNE   FLT19                                                            
         TM    BYTE,X'40'          CONFIRMED NOW?                               
         BO    FLTNO                                                            
         CLC   DUB+1(1),DUB        STATION VERSION HIGHER?                      
         BNH   FLTNO                                                            
         TM    DUB+2,X'30'         VER # NOT ADVANCED?                          
         BO    FLTOK               OK                                           
         B     FLTNO               ADVANCED-REJECT                              
*                                                                               
FLT19    CLC   =C'UR',TBLCTYP      UR-REP VERSION - NO WIPS                     
         BNE   FLTOK               SHOULD NEVER GET HERE                        
*                                  BUT LET'S NOT BLOW UP                        
         TM    BYTE,X'40'          CONFIRMED NOW?                               
         BO    FLTNO                                                            
         CLC   DUB(1),DUB+1        REP VERSION HIGHER?                          
         BNH   FLTNO                                                            
         TM    DUB+2,X'30'         VER # NOT ADVANCED?                          
         BO    FLTOK               OK                                           
         B     FLTNO               ADVANCED-REJECT                              
*                                                                               
FLTOK    SR    RB,RB                                                            
FLTNO    LTR   RB,RB                                                            
         XIT1                                                                   
*                                                                               
                                                                                
                                                                                
************************************************************                    
* THIS ROUTINE ONLY CALLED FROM FILTERS                                         
GETSEND  DS    0H                     GET X'20' ELEMENT                         
         L     R4,AIOAREA                                                       
         USING RCONREC,R4                                                       
         LA    R4,RCONELEM                                                      
GTS10    ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    FLTNO                                                            
         AR    R4,R1                                                            
         CLI   0(R4),X'20'         ELEMENT FOR SEND INFO                        
         BH    FLTNO                                                            
         BNE   GTS10                                                            
         BR    RE                                                               
         DROP  R4                                                               
                                                                                
* THIS ROUTINE ONLY CALLED FROM FILTERS                                         
GETSEND2 DS    0H                     GET X'21' ELEMENT                         
         L     R4,AIOAREA                                                       
         USING RCONREC,R4                                                       
         LA    R4,RCONELEM                                                      
GTS102   ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    FLTNO                                                            
         AR    R4,R1                                                            
         CLI   0(R4),X'21'         ELEMENT FOR SEND INFO                        
         BH    FLTNO                                                            
         BNE   GTS102                                                           
         BR    RE                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
* FILTERS FOR 9D KEY-READ THAT ARE HANDLED BY REPIO IN 8D/8E READ               
SPECFLT  NTR1                                                                   
         CLI   RIPCAT,X'40'            CATEGORY FILTER                          
         BNH   SPC10                                                            
         L     R4,AIOAREA                                                       
         USING RCONREC,R4                                                       
         LA    R4,RCONELEM                                                      
         USING RCONELEM,R4                                                      
         CLC   RIPCAT(1),RCONCTGY                                               
         BNE   FLTNO                                                            
         CLI   RIPCAT+1,X'40'                                                   
         BNH   FLTOK                                                            
         CLC   RIPCAT+1(1),RCONCTGY+1                                           
         BNE   FLTNO                                                            
         B     SPC10                                                            
SPC10    EQU   *                                                                
         B     FLTOK                                                            
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* SET MAXIOCTR TO 90% OF MAX IO'S SHOWN BY GETFACT                              
SETMAXCT NTR1                                                                   
         LA    R2,REPIOTBL                                                      
         USING REPIOD,R2                                                        
         XC    RIPFULL,RIPFULL     CLEAR SEQ/HIGH REPIO COUNTER                 
                                                                                
         L     R5,ACOMFACS         NO                                           
         USING COMFACSD,R5                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         MVC   MAXIOCTR,FATMAXIO-FACTSD(R1)                                     
         SR    R2,R2                                                            
         LH    R3,MAXIOCTR                                                      
         LA    R4,9                MULTIPLY MAX IO BY 9                         
         MR    R2,R4                                                            
         LA    R4,10               DIVIDE MAXIMUM BY 10                         
         DR    R2,R4                                                            
         STH   R3,MAXIOCTR                                                      
MAXXIT   XIT1                                                                   
                                                                                
*                                                                               
* CHECK IO'S AGAINST MAXIOCTR                                                   
* RETURN VIA R4 TO CALLER                                                       
CHKMAXCT DS    0H                                                               
         L     R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         CLC   MAXIOCTR,FATIOCNT-FACTSD(R1)                                     
         BH    MAXOK                                                            
         MVI   DMCB+1,X'FF'        SET MAXED OUT                                
         B     MAXXIT                                                           
MAXOK    BR    R4                                                               
         DROP  R5                                                               
       ++INCLUDE RGENEROL                                                       
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RERISWRK                                                       
       ++INCLUDE REPIOBLK                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019RERIS0AS  05/01/02'                                      
         END                                                                    

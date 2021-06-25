*          DATA SET PRSFM29    AT LEVEL 094 AS OF 07/21/16                      
*PHASE T41C29A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C29 - PRODUCT GROUP MAINTENANCE/LIST'                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 07/20/16 Fix starting media code for LIST                                
*                                                                               
* BOBY 06/00/06 SECURITY FOR PRODUCT GROUPS                                     
*                                                                               
* KWAN 07/08/02 BUG FIX FOR COUNTER PROBLEM (N-N OF N DISPLAYED)                
*                                                                               
* KWAN 06/07/02 NOW REPORT, MAKE ID FIELD REQUIRED                              
*                                                                               
* KWAN 05/13/02 FOR REPORT ACTION, PRINT PRD NAME                               
*                                                                               
* KWAN 10/00    FIX DISPLAY PROBLEM WHEN CHANGING PRODUCT LIST ITEM(S)          
*                                                                               
* BPLA 09/95    FIX BUG AT VR65  - REMOVE OLD ELEM RIGHT AFTER                  
*               READING IT AT VR62                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST  *         
*                                                                     *         
*  INPUTS       SCREEN T41CB4 (MAINTENANCE)                           *         
*               SCREEN T41CBD (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED GROUP DEFINITION RECORDS                      *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- MINBLKD                                         *         
*               R6 -- WORK                                            *         
*               R7 -- SECOND BASE                                     *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - PRODUCT GROUP RECORD                            *         
*               IO2 - GROUP DEFINITION RECORD                         *         
*               IO3 - MINIO RECORD TABLE                              *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41C29   CSECT                                                                  
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,T41C29,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
INT      BAS   RE,INIT             INITIALIZE                                   
*                                                                               
* IF FOLLOWING COMPARE IS NOT MADE, PF12 WILL NOT DISPLAY FIRST                 
* SELECTED RECORD (I.E. PF12 IS USED INSTEAD OF NORMAL ENTER)                   
*                                                                               
         CLI   ACTNUM,ACTLIST      IS LIST?                                     
         BNE   *+8                                                              
         MVI   PFAID,0             SET PFKEY SAME AS ENTER                      
*                                                                               
         CLI   ACTNUM,ACTSEL       SELECT?                                      
         BNE   CM                                                               
         TM    GENSTAT2,NEXTSEL                                                 
         JO    JUMPXIT1                                                         
         CLI   PFAID,12            PF 12 OR 24 FOR RETURN?                      
         BE    RTN                                                              
         CLI   PFAID,24                                                         
         BNE   STY                                                              
RTN      OI    GENSTAT2,NEXTSEL+RETEQSEL                                        
         XC    SFCSTRT,SFCSTRT                                                  
         MVI   PFAID,0                                                          
         J     JUMPXIT1                                                         
STY      OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CM       CLI   ACTNUM,ACTDEL       DELETE NOT ALLOWED                           
         BE    CMERR                                                            
         CLI   ACTNUM,ACTREST      RESTORE NOT ALLOWED                          
         BNE   CM50                                                             
CMERR    LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
CM50     CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       ONLINE REPORT                                
         BE    PR                                                               
*                                                                               
JUMPXIT1 XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
INIT     NTR1                                                                   
         MVI   IOOPT,C'Y'          GENCON DOES NO WRITES                        
******   MVI   ACTELOPT,C'N'       DON'T WRITE 'F1' ELEMENTS                    
         OI    GENSTAT4,NODELLST   NO DELETIONS FROM LIST                       
*                                                                               
         OI    SFCNM1H+6,X'81'     CHA TO MODIFIED FLD (GAIN CONTROL)           
*                                                                               
         BRAS  RE,PID              SET USER'S PID AND SECURITY AGY              
*                                                                               
INITX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        VALIDATE KEY                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                                                               
         XC    STRTCODE(SFLNQ),STRTCODE                                         
         XC    BREAK1LN(BKLNQ),BREAK1LN                                         
         MVC   SVBK1NM(SVBKLQ),SPACES                                           
*                                                                               
         CLI   ACTNUM,ACTLIST      IF LIST, CLEAR NAME FIELDS                   
         BNE   VK05                                                             
         MVC   SFLMEDN,SPACES                                                   
         OI    SFLMEDNH+6,X'80'                                                 
         MVC   SFLCLTN,SPACES                                                   
         OI    SFLCLTNH+6,X'80'                                                 
*                                                                               
VK05     XC    WKKEY,WKKEY                                                      
         LA    R4,WKKEY            READ TO BUILD KEY                            
         USING GRPKEY,R4                                                        
         MVC   GRPKAGY,AGENCY                                                   
         MVI   GRPKRCOD,GRPKPTYQ                                                
*                                                                               
         MVI   ERROR,MISSING                                                    
         LA    R2,SFCMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK10                                                             
         GOTO1 VALIMED                                                          
         CLI   ACTNUM,ACTLIST      LIST SCR HAS MEDIA NAME FIELD                
         BNE   *+20                                                             
         MVC   SFLMEDN,SPACES                                                   
         MVC   SFLMEDN(L'MEDNM),MEDNM                                           
         OI    SFLMEDNH+6,X'80'                                                 
         MVC   GRPKMED,QMED                                                     
         MVC   FLTRMED,QMED        FOR FILTERING ON MEDIA (LIST)                
*                                                                               
VK10     LA    R2,SFCCLTH                                                       
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK13                                                             
         CLI   5(R2),0                                                          
         BE    VK15                OPTIONAL FOR LIST                            
         CLI   FLTRMED,0                                                        
         BNE   VK13                HAVE MEDIA, CLT CAN BE VALIDATED             
         MVC   FLTRCLT,SPACES                                                   
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLTRCLT(0),8(R2)    MOVE WHATEVER IN CLT FLD TO FILTER           
         B     VK15                                                             
VK13     GOTO1 VALICLT                                                          
         CLI   ACTNUM,ACTLIST      LIST SCR HAS CLIENT NAME FIELD               
         BNE   *+20                                                             
         MVC   SFLCLTN,SPACES                                                   
         MVC   SFLCLTN(L'SFLCLTN),CLTNM                                         
         OI    SFLCLTNH+6,X'80'                                                 
         MVC   GRPKCLT,QCLT                                                     
         MVC   FLTRCLT,QCLT        FOR FILTERING ON CLT (LIST)                  
*                                                                               
VK15     LA    R2,SFCIDH           GROUP ID                                     
         CLI   ACTNUM,ACTREP       REPORT                                       
         BNE   VKI                                                              
         CLI   5(R2),0             ENTERED                                      
         BE    VB                                                               
         TM    4(R2),X'04'         ALPHABETIC CHARACTER?                        
         BO    *+12                                                             
         MVI   ERROR,NOTALPHA                                                   
         B     TRAPERR                                                          
         MVC   GRPID,8(R2)                                                      
         B     VK60                                                             
VB       MVI   GRPID,C' '                                                       
         B     VK60                                                             
*                                                                               
VKI      CLI   5(R2),0                                                          
         BNE   VK27                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK35                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK27     TM    4(R2),X'04'         ALPHABETIC CHARACTER?                        
         BO    *+12                                                             
         MVI   ERROR,NOTALPHA                                                   
         B     TRAPERR                                                          
         MVC   GRPID,8(R2)                                                      
         MVC   FLTRID,8(R2)        FOR FILTERING ON ID (LIST)                   
*                                                                               
         CLI   FLTRMED,0           ANYTHING IN MEDIA FILTER?                    
         BE    VK35                                                             
         OC    FLTRCLT,FLTRCLT     ANYTHING IN CLIENT FILTER?                   
         BZ    VK35                                                             
*                                                                               
         MVC   GRPKID,GRPID        PUT ID IN KEY                                
         MVC   KEY(25),WKKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+12                                                             
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
         MVC   AIO,AIO2            READ GROUP DEFINITION REC INTO IO2           
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GRPBRKD,R6                                                       
         MVC   BREAK1LN,GRPBK1LN   SAVE BREAK LENGTHS                           
         MVC   BREAK2LN,GRPBK2LN                                                
*                                                                               
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK28                                                             
*                                                                               
         MVC   SFCBK1(L'SFCBK1),SPACES                                          
         MVC   SFCBK1(L'SFCBK1),GRPBK1                                          
         OI    SFCBK1H+6,X'80'                                                  
         MVC   SFCBK1L(L'SFCBK1L),SPACES                                        
         MVI   SFCBK1L,C'('                                                     
         MVC   SFCBK1L+1(1),GRPBK1LN                                            
         OI    SFCBK1L+1,X'F0'                                                  
         MVI   SFCBK1L+2,C')'                                                   
         OI    SFCBK1LH+6,X'80'                                                 
*                                                                               
         MVC   SFCBK2(L'SFCBK2),SPACES                                          
         OI    SFCBK2H+6,X'80'                                                  
         MVC   SFCBK2L(L'SFCBK2L),SPACES                                        
         OI    SFCBK2LH+6,X'80'                                                 
         CLI   GRPBK2LN,0          BREAK 2 MAY NOT BE THERE                     
         BE    VK30                                                             
         MVC   SFCBK2(L'SFCBK2),GRPBK2                                          
         OI    SFCBK2H+6,X'80'                                                  
         MVI   SFCBK2L,C'('                                                     
         MVC   SFCBK2L+1(1),GRPBK2LN                                            
         OI    SFCBK2L+1,X'F0'                                                  
         MVI   SFCBK2L+2,C')'                                                   
         OI    SFCBK2LH+6,X'80'                                                 
         B     VK30                                                             
*                                                                               
VK28     MVC   SVBK1NM,GRPBK1      SAVE THOSE VALUES TO SET UP                  
         MVC   SVBK1L,GRPBK1LN     LIST SCREEN IN LR MODE                       
         MVC   SVBK2NM,GRPBK2                                                   
         MVC   SVBK2L,GRPBK2LN                                                  
*                                                                               
VK30     DS    0H                                                               
*                                                                               
         XC    SVSECELM,SVSECELM   INIT SECURITY ELM SAVEAREA                   
*                                                                               
         L     R6,AIO2             POINT TO GRP DEFN RECORD                     
         MVI   ELCODE,GRPSECCQ     READ SECURITY ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVSECELM,0(R6)      SAVE SECURITY ELEMENT                        
*                                                                               
         CLI   ACTNUM,ACTDIS       SKIP IF ONLY DISPLAYING                      
         BE    *+8                                                              
         CLI   ACTNUM,ACTLIST      LISTING                                      
         BE    *+8                                                              
         CLI   ACTNUM,ACTREP       LISTING                                      
         BE    *+8                                                              
         BRAS  RE,CHKSEC           CHECK IF USER CAN MAKE CHANGES               
*                                                                               
VKSECX   DS    0H                                                               
*                                                                               
         MVC   GRPKID,GRPID        PUT ID IN KEY                                
         MVC   FLTRID,GRPID        ID IS NOW A FILTER (LIST)                    
*                                                                               
VK35     LA    R2,SFCCODEH         CODE                                         
         CLI   5(R2),0                                                          
         BNE   VK40                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         MVC   STRTCODE,=X'0001'   SET FOR FIRST GROUP RECORD                   
         B     VK60                                                             
*                                                                               
VK40     TM    4(R2),X'08'         NUMERIC?                                     
         BO    *+12                                                             
         MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
*                                                                               
         OC    BREAK1LN(BKLNQ),BREAK1LN                                         
         BZ    VK50                                                             
*                                                                               
         ZIC   R0,BREAK1LN         GET SUM OF BREAK LENGTHS                     
         ZIC   R1,BREAK2LN                                                      
         AR    R0,R1                                                            
         CLM   R0,1,5(R2)          NUMB OF DIGITS MUST EQUAL TO SUM             
         BNE   BADNUMDG                                                         
VK50     MVC   FULL,8(R2)          GROUP CODES ARE LEFT-JUSTIFIED, PWOS         
         OC    FULL,=C'0000'                                                    
         PACK  DUB,FULL                                                         
         L     R0,DUB+4                                                         
         SRA   R0,4                                                             
         BZ    BADGRPCD            AND MUST BE NON-ZERO                         
         STCM  R0,3,GRPCODE                                                     
         MVC   GRPKCODE,GRPCODE                                                 
         MVC   STRTCODE,GRPCODE    FOR FILTERING ON CODE (LIST)                 
*                                                                               
VK60     XC    KEY,KEY                                                          
         MVC   KEY(10),WKKEY       THROUGH CODE (IF GIVEN)                      
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
VKX      J     JUMPXIT1                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        VALIDATE RECORD                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                                                               
*                                                                               
         BRAS  RE,CHKSEC           CHECK IF USER CAN MAKE CHANGES               
*                                                                               
         MVI   VRSW,C' '           NO FIELD HAS BEEN MODIFIED YET               
         LA    R2,SFCNM1H          NAME 1 FIELD                                 
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SVNAME1     COMPARING WITH PREVIOUS VALUE                
         BE    *+8                                                              
         MVI   VRSW,C'Y'           FIELD HAS BEEN MODIFIED                      
*                                                                               
         LA    R2,SFCNM2H          NAME 2 FIELD                                 
         CLI   BREAK2LN,0                                                       
         BNE   *+16                                                             
         CLI   5(R2),0                                                          
         BE    VR10                                                             
         B     NOBREAK2                                                         
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SVNAME2     COMPARING WITH PREVIOUS VALUE                
         BE    *+8                                                              
         MVI   VRSW,C'Y'           FIELD HAS BEEN MODIFIED                      
*                                                                               
VR10     LA    R2,SFCNEWH          LIST OF STATNS/CLTS TO BE VALIDATED          
         MVI   VALUES,X'FF'        INITIALIZE VALUES WITH X'FF'                 
         MVC   VALUES+1(L'VALUES-1),VALUES                                      
         LA    R3,VALUES                                                        
         USING VALUED,R3                                                        
         LA    R4,KEY                                                           
         USING GRPPKEY,R4                                                       
*                                                                               
VR20     CLI   5(R2),0                                                          
         BE    VR60                                                             
*                                                                               
         XC    MYWORK,MYWORK       BUILD FAKE TWA FIELD                         
         MVI   MYWORK,16+4         HEADER + EXT + 4-BYTE FIELD                  
         ZIC   R1,5(R2)                                                         
         LA    RF,8(R2)                                                         
         MVI   VALUEACT,C'+'                                                    
         CLI   0(RF),C'-'                                                       
         BNE   *+14                                                             
         MVI   VALUEACT,C'-'                                                    
         BCTR  R1,0                                                             
         LA    RF,1(RF)                                                         
         STC   R1,MYWORK+5                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYWORK+8(0),0(RF)                                                
         ST    R2,ACURFORC         IN CASE OF ERROR                             
         LA    R2,MYWORK                                                        
*                                                                               
         MVC   VALUEVAL,SPACES     BLANK-PAD ALL VALUES                         
         XC    VALUEDEL,VALUEDEL   ASSUME NO DELETE                             
*                                                                               
         GOTO1 VALIPRD             VALIDATE THE PRODUCT                         
         MVC   VALUEVAL(3),QPRD                                                 
         B     VR40                                                             
*                                                                               
VR40     DS    0H                                                               
         L     R2,ACURFORC                                                      
         XC    KEY,KEY                                                          
         MVI   GRPPTYP,GRPPPGQ                                                  
         MVC   GRPPAGY,AGENCY                                                   
         MVC   GRPPMED,QMED                                                     
         MVC   GRPPCLT,QCLT                                                     
         MVC   GRPPVAL,VALUEVAL                                                 
         MVC   GRPPID,GRPID                                                     
         MVC   GRPPCODE,GRPCODE                                                 
*                                                                               
         MVI   DMINBTS,X'08'       LOOK FOR PASSIVE - READ FOR DELETES          
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         TM    DMCB+8,X'02'        PASSIVE MAY BE DELETED                       
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   DMINBTS,0                                                        
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+16                                                             
         CLI   VALUEACT,C'+'                                                    
         BE    VR50                NOT PRESENT -- OK TO ADD                     
         B     NODELETE            . . . BUT CAN'T DELETE IT                    
*                                                                               
         TM    GRPPCNTL,X'80'      WAS POINTER DELETED?                         
         BZ    *+16                NO                                           
         CLI   VALUEACT,C'+'                                                    
         BE    VR50                                                             
         B     NODELETE            CAN'T DELETE -- IT'S ALREADY GONE            
         CLI   VALUEACT,C'-'                                                    
         BE    VR50                                                             
         B     NOADD               CAN'T ADD -- IT'S ALREADY THERE              
*                                                                               
VR50     LA    R1,VALUES                                                        
LP       CR    R1,R3                                                            
         BE    VR55                                                             
         CLC   VALUEVAL,0(R1)      DID THEY GIVE SAME ONE TWICE?                
         BE    NODUP               ERROR                                        
         LA    R1,VALUELNQ(R1)                                                  
         B     LP                                                               
VR55     LA    R3,VALUELNQ(R3)     BUMP TO NEXT ENTRY IN VALUE TABLE            
         MVI   VRSW,C'Y'           FIELD HAS BEEN MODIFIED                      
*                                                                               
VR60     ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         CLI   0(R2),17            ANY MORE FIELDS?                             
         BH    VR20                YES                                          
*                                                                               
* RECORD HAS NOW BEEN VALIDATED                                                 
*                                                                               
         EJECT                                                                  
*                                                                               
* ADD GROUP NAME RECORD (HAS GROUP CODE IN KEY)                                 
*                                                                               
         L     R3,AIO1                                                          
         XC    0(250,R3),0(R3)     CLEAR AIO                                    
         MVC   AIO,AIO1                                                         
*                                                                               
* MUST REREAD RECORD SINCE VALICLT OR VALIPUB READ OVER IT                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,WKKEY           DOESN'T HAVE GROUP CODE                      
         MVC   GRPKCODE,GRPCODE                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    VR62                                                             
         B     VR62X                                                            
*                                                                               
VR62     DS    0H                                                               
         GOTO1 GETREC                                                           
         MVI   ELCODE,GRPGRPCQ                                                  
         GOTO1 REMELEM                                                          
*                                                                               
VR62X    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING GRPGRPD,R6                                                       
         MVI   GRPGRPCD,GRPGRPCQ                                                
         MVI   GRPGRPLN,GRPGRPLQ                                                
         MVC   GRPGNAM1,SFCNM1                                                  
         MVC   GRPGNAM2,SFCNM2                                                  
         L     R3,AIO                                                           
         CLC   0(25,R3),KEYSAVE    SEE IF I HAVE THE RECORD                     
         BE    VR65                                                             
         XC    0(150,R3),0(R3)     CLEAR RECORD AREA                            
         MVC   0(25,R3),KEYSAVE    SET KEY IN RECORD                            
         MVI   26(R3),33+GRPGRPLQ                                               
         MVC   33(GRPGRPLQ,R3),ELEM                                             
         GOTO1 ADDREC                                                           
         B     VR80                                                             
*                                                                               
VR65     DS    0H                                                               
         GOTO1 ADDELEM                                                          
         GOTO1 PUTREC                                                           
*                                                                               
VR80     LA    R3,VALUES           LIST OF SAVED PRODUCTS                       
*                                                                               
VR90     CLI   0(R3),X'FF'         ANY MORE VALUES TO PROCESS?                  
         BE    VR160               NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   GRPPTYP,GRPPPGQ                                                  
         MVC   GRPPAGY,AGENCY                                                   
         MVC   GRPPMED,QMED                                                     
         MVC   GRPPCLT,QCLT                                                     
         MVC   GRPPVAL,VALUEVAL                                                 
         MVC   GRPPID,GRPID                                                     
         MVC   GRPPCODE,GRPCODE                                                 
*                                                                               
         MVI   DMINBTS,X'08'       LOOK FOR PASSIVE - READ FOR DELETES          
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         TM    DMCB+8,X'02'        PASSIVE MAY BE DELETED                       
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   DMINBTS,0                                                        
         CLC   KEY(25),KEYSAVE     IS THE PASSIVE ALREADY THERE?                
         BE    VR110                                                            
*                                                                               
         CLI   VALUEACT,C'+'       NO -- THIS MUST BE AN ADD                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   MYWORK(32),KEY      SAVE KEY                                     
         XC    KEY+14(2),KEY+14    CLEAR GROUP CODE                             
         GOTO1 HIGH                                                             
         CLC   KEY(14),KEYSAVE     IS VALUE ASSIGNED SOMEWHERE ELSE?            
         BNE   VR100                                                            
         OI    GRPPCNTL,X'80'      YES -- DELETE THAT POINTER                   
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   VALUEDEL,GRPPCODE   MUST DELETE AN ELEMENT LATER                 
*                                                                               
VR100    XC    KEY,KEY                                                          
         MVC   KEY(32),MYWORK      RESTORE KEY                                  
         MVC   GRPPCNTL(2),=X'00FF'    DIRECTORY ONLY RECORD                    
*                                                                               
         GOTO1 ADD                 ADD THE PASSIVE POINTER                      
         CLI   DMCB+8,0                                                         
         BE    VR130               ADD THE MINIO ELEMENT                        
         DC    H'0'                                                             
*                                                                               
VR110    TM    GRPPCNTL,X'80'      WAS POINTER DELETED?                         
         BZ    VR140                                                            
         CLI   VALUEACT,C'+'       YES -- THIS MUST BE AN ADD                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MYWORK(32),KEYSAVE  SAVE KEY                                     
         XC    KEY+14(2),KEY+14    CLEAR GROUP CODE                             
         GOTO1 HIGH                                                             
         CLC   KEY(14),KEYSAVE     IS VALUE ASSIGNED SOMEWHERE ELSE?            
         BNE   VR120                                                            
         OI    GRPPCNTL,X'80'      YES -- DELETE THAT POINTER                   
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   VALUEDEL,GRPPCODE   MUST DELETE AN ELEMENT LATER                 
*                                                                               
VR120    XC    KEY,KEY                                                          
         MVC   KEY(32),MYWORK                                                   
         NI    GRPPCNTL,X'FF'-X'80'                                             
         MVI   GRPPCNTL+1,X'FF'    SET DIRECTORY ONLY INDICATOR                 
         GOTO1 WRITE               RESTORE RECORD                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR130    DS    0H                                                               
         B     VR150                                                            
*                                                                               
VR140    CLI   VALUEACT,C'-'       THEY MUST BE DELETING                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    GRPPCNTL,X'80'      DELETE THE POINTER                           
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR150    LA    R3,VALUELNQ(R3)     BUMP TO NEXT SAVED PRODUCT                   
         B     VR90                                                             
*                                                                               
VR160    DS    0H                                                               
*                                                                               
         LA    R3,VALUES           LOOK FOR WRAP-UP DELETIONS                   
*                                                                               
VR170    CLI   0(R3),X'FF'         END OF LIST?                                 
         BE    VR190                                                            
*                                                                               
         OC    VALUEDEL,VALUEDEL   ANYTHING TO DELETE?                          
         BZ    VR180               NO                                           
*                                                                               
VR180    LA    R3,VALUELNQ(R3)     BUMP TO NEXT VALUE IN LIST                   
         B     VR170                                                            
         EJECT                                                                  
*                                  NOW PROCESS GROUP/VALUE POINTERS             
VR190    DS    0H                                                               
VR280    LA    R3,VALUES           LIST OF SAVED PRODUCTS                       
*                                                                               
VR290    CLI   0(R3),X'FF'         ANY MORE VALUES TO PROCESS?                  
         BE    VR360               NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   GRPGTYP,GRPGPGQ                                                  
         MVC   GRPGAGY,AGENCY                                                   
         MVC   GRPGMED,QMED                                                     
         MVC   GRPGCLT,QCLT                                                     
         MVC   GRPGVAL,VALUEVAL                                                 
         MVC   GRPGID,GRPID                                                     
         MVC   GRPGCODE,GRPCODE                                                 
*                                                                               
         MVI   DMINBTS,X'08'       LOOK FOR PASSIVE - READ FOR DELETES          
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+14                                                             
         TM    DMCB+8,X'02'        PASSIVE MAY BE DELETED                       
         BO    *+6                                                              
         DC    H'0'                                                             
         MVI   DMINBTS,0                                                        
         CLC   KEY(25),KEYSAVE     IS THE PASSIVE ALREADY THERE?                
         BE    VR310                                                            
*                                                                               
         CLI   VALUEACT,C'+'       NO -- THIS MUST BE AN ADD                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  SEE IF ASSIGNED ELSWHERE                     
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   MYWORK(32),KEY      SAVE KEY                                     
*                                  (OLD GROUP SHOULD BE IN VALUEDEL)            
         OC    VALUEDEL,VALUEDEL                                                
         BZ    VR300                                                            
*                                                                               
         MVC   GRPGCODE,VALUEDEL                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(14),KEYSAVE     IS VALUE ASSIGNED SOMEWHERE ELSE?            
         BNE   VR300                                                            
         OI    GRPGCNTL,X'80'      YES -- DELETE THAT POINTER                   
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR300    XC    KEY,KEY                                                          
         MVC   KEY(32),MYWORK      RESTORE KEY                                  
         MVC   GRPGCNTL(2),=X'00FF'    DIRECTORY ONLY RECORD                    
         GOTO1 ADD                 ADD THE PASSIVE POINTER                      
         CLI   DMCB+8,0                                                         
         BE    VR330               ADD THE MINIO ELEMENT                        
         DC    H'0'                                                             
         B     VR350                                                            
*                                                                               
VR310    TM    GRPGCNTL,X'80'      WAS POINTER DELETED?                         
         BZ    VR340                                                            
         CLI   VALUEACT,C'+'       YES -- THIS MUST BE AN ADD                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MYWORK(32),KEYSAVE  SAVE KEY                                     
         OC    VALUEDEL,VALUEDEL     SEE IF WAS ASSIGNED ELSEWHERE              
         BZ    VR320                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(32),MYWORK                                                   
         MVC   GRPGCODE,VALUEDEL                                                
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(14),KEYSAVE     IS VALUE ASSIGNED SOMEWHERE ELSE?            
         BNE   VR320                                                            
         OI    GRPGCNTL,X'80'      YES -- DELETE THAT POINTER                   
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR320    XC    KEY,KEY                                                          
         MVC   KEY(32),MYWORK                                                   
         NI    GRPGCNTL,X'FF'-X'80'                                             
         MVI   GRPPCNTL+1,X'FF'    SET DIRECTORY ONLY INDICATOR                 
         GOTO1 WRITE               RESTORE RECORD                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR330    DS    0H                                                               
         B     VR350                                                            
*                                                                               
VR340    CLI   VALUEACT,C'-'       THEY MUST BE DELETING                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    GRPGCNTL,X'80'      DELETE THE POINTER                           
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR350    LA    R3,VALUELNQ(R3)     BUMP TO NEXT SAVED PRODUCT                   
         B     VR290                                                            
*                                                                               
VR360    DS    0H                                                               
*                                                                               
         LA    R3,VALUES           LOOK FOR WRAP-UP DELETIONS                   
*                                                                               
VR370    CLI   0(R3),X'FF'         END OF LIST?                                 
         BE    VR390                                                            
*                                                                               
VR380    LA    R3,VALUELNQ(R3)     BUMP TO NEXT VALUE IN LIST                   
         B     VR370                                                            
         DROP  R3                                                               
*                                                                               
VR390    DS    0H                                                               
*                                                                               
VR690    LA    R2,SFCNEWH          CLEAR INPUT FIELDS                           
*                                                                               
VR700    CLI   5(R2),0             IF ANYTHING IS IN FIELD,                     
         BE    VR710                                                            
         ZIC   R1,0(R2)            . . . THEN CLEAR IT                          
         AHI   R1,-17              L'HDR + L'EXT + 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
VR710    OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),17            ANY MORE FIELDS TO CLEAR?                    
         BH    VR700               MAYBE                                        
*                                                                               
VRX      B     DR                                                               
*                                                                               
VRSW     DS    X                   VALREC SWITCH                                
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        DISPLAY RECORD                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                                                               
         LA    R2,SFCLISTH                                                      
         TWAXC (R2),PROT=Y,TRNS=T  CLEAR LIST FIELDS                            
*                                                                               
         MVI   BYTE,0                                                           
         BAS   RE,CNTPLST          GET TOTAL NUMBER OF LIST MEMBERS             
*                                                                               
         CLI   ACTNUM,ACTSEL       SELECT?                                      
         BNE   *+20                                                             
         MVC   SFCPFK,SPACES                                                    
         MVC   SFCPFK(20),=C'PF12=Next Selection^'                              
         OI    SFCPFKH+6,X'80'                                                  
*                                                                               
         CLI   THISLSEL,C'R'       RESTORE SEL CODE?                            
         BNE   DR05                                                             
         MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD+00(21),=C'Record is not deleted'                         
         MVC   CONHEAD+21(22),=C', hit PF12 to continue'                        
         OI    CONHEADH+6,X'80'                                                 
         LA    R2,CONACTH                                                       
         OI    GENSTAT2,USMYOK                                                  
         B     GOERREX2                                                         
*                                                                               
DR05     L     R6,AIO                                                           
         MVI   ELCODE,GRPGRPCQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                SOMETHING VERY WRONG                         
*                                                                               
         USING GRPGRPD,R6                                                       
DR10     MVC   SFCNM1,GRPGNAM1     DISPLAY GROUP NAMES                          
         OI    SFCNM1H+6,X'80'                                                  
         MVC   SFCNM2,GRPGNAM2                                                  
         OI    SFCNM2H+6,X'80'                                                  
         MVC   SVNAME1,GRPGNAM1    SAVING NAMES FOR VR USES                     
         MVC   SVNAME2,GRPGNAM2                                                 
         DROP  R6                                                               
*                                                                               
* DISPLAY PRODUCTS FROM POINTERS IN LARGE PROTECTED FIELDS (76 CHARS)           
*                                                                               
         XC    DISP,DISP                                                        
         MVC   DISLEN,=H'3'        FOR PRODUCTS                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPGKEY,R4                                                       
         MVC   GRPGAGY,AGENCY                                                   
         MVC   GRPGMED,QMED                                                     
         MVI   GRPGTYP,GRPGPGQ                                                  
         MVC   GRPGCLT,QCLT                                                     
         MVC   GRPGID,GRPID                                                     
         MVC   GRPGCODE,GRPCODE                                                 
         CLI   SFCSTRTH+5,0                                                     
         BNE   *+10                                                             
         XC    SFCSTRT,SFCSTRT                                                  
         MVC   GRPGVAL(3),SFCSTRT  LAST VALUE ALREADY DISPLAYED                 
*                                                                               
         MVI   POSDSPSW,0          SWITCH FOR POSTION DISPLAYING                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
DR20     DS    0H                                                               
         CLC   KEY(10),KEYSAVE     CHECK THROUGH GROUP ID                       
         BE    DR25                                                             
*                                                                               
         MVI   BYTE,C'P'           TELL SUBROUTINE TO DO POSITIONING            
         BAS   RE,CNTPLST          WILL RETURN POSNUMB (FULL WORD)              
*                                                                               
         LA    R1,SFCPFKH                                                       
         LA    R1,51(R1)           MOVE OVER 51 SPACES                          
         CLC   POSNUMB,NUMBMEM                                                  
         BNH   *+6                                                              
         DC    H'0'                POSITION NUMB HAS TO BE EQ OR LOWER          
*                                                                               
         L     RF,POSNUMB          CHECK IF NO MEMBER FOUND                     
         CHI   RF,0                                                             
         BE    DR23C                                                            
         L     RF,NUMBMEM          CHECK IF ZERO MEMBER IN LIST                 
         CHI   RF,0                                                             
         BNE   DR23E                                                            
DR23C    MVI   0(R1),C'0'                                                       
         LA    R1,2(R1)            MOVE OVER 2 SPACES                           
         MVC   0(6,R1),=C'member'                                               
         LA    R1,7(R1)            MOVE OVER 7 SPACES                           
         B     DR23P                                                            
*                                                                               
DR23E    DS    0H                                                               
         L     RF,POSNUMB                                                       
         L     R6,NUMBMEM                                                       
*                                                                               
         EDIT  (B4,POSNUMB),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,      +        
               ALIGN=LEFT                                                       
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         MVI   0(R1),C'-'                                                       
         LA    R1,1(R1)            MOVE OVER A SPACE                            
         EDIT  (R6),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,ALIGN=LEFT             
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         LA    R1,1(R1)            MOVE OVER A SPACE                            
         MVC   0(2,R1),=C'of'                                                   
         LA    R1,3(R1)            MOVE OVER 3 SPACES                           
         EDIT  (B4,NUMBMEM),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,      +        
               ALIGN=LEFT                                                       
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         LA    R1,1(R1)            MOVE OVER A SPACE                            
DR23P    MVC   0(9,R1),=C'displayed'                                            
*                                                                               
         XC    SFCSTRT,SFCSTRT                                                  
*                                                                               
DRACTV   DS    0H                  DISPLAY ACTIVITY                             
*                                                                               
         MVI   ELCODE,X'F1'        SET ACTIVITY ELEMENT CODE                    
         L     R6,AIO              POINT TO RECORD                              
         BRAS  RE,GETEL            FIND ACTIVITY ELEMENT                        
         BNE   DRACTVX             SKIP IF NONE FOUND                           
*                                                                               
         LA    R2,SFCLUPDH         ASSUME CLIENT GROUPS                         
         MVC   8(13,R2),=CL13'Last Updated:'                                    
         OI    6(R2),X'80'         TRANSMIT FLD                                 
*                                                                               
         USING ACTVD,R6            ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         LA    R3,ACTVCHDT         ASSUME LAST CHANGED DATE                     
         OC    ACTVCHDT,ACTVCHDT   IF NO LAST CHANGED DATE                      
         BNZ   *+8                                                              
         LA    R3,ACTVADDT            USE ADDED DATE                            
*                                                                               
         OC    0(3,R3),0(R3)       SKIP IF NO DATE AVAILABLE                    
         BZ    DRACTVX                                                          
*                                                                               
         LA    R2,SFCDTEUH         ASSUME CLIENT GROUPS                         
*                                                                               
         GOTOR DATCON,DMCB,(3,0(R3)),(17,8(R2)) DISP DATE                       
         OI    6(R2),X'80'         TRANSMIT FLD                                 
*                                                                               
         LA    R2,SFCPIDUH         POINT TO LAST CHANGED FIELD                  
*                                                                               
         LA    R3,ACTVCHID         ASSUME LAST CHANGED PID                      
         OC    ACTVCHID,ACTVCHID   IF NO LAST CHANGED PID                       
         BNZ   *+8                                                              
         LA    R3,ACTVADID            USE ADDED PID                             
*                                                                               
         GOTOR TRNPID,DMCB,0(R3)   DISPLAY PID                                  
*                                                                               
DRACTVX  DS    0H                                                               
*                                                                               
         B     DR80                                                             
*                                                                               
DR25     DS    0H                                                               
         CLI   POSDSPSW,C'Y'       POSTION DISPLAYING SWITCH ON?                
         BE    *+14                                                             
         MVC   POSVALUE,GRPGVAL    NEEDED FOR VALUE TO BE POSITIONED            
         MVI   POSDSPSW,C'Y'       TURN SWITCH ON                               
*                                                                               
         LR    R1,R2               SEE IF I CAN FIT ON THIS LINE                
         LA    R1,8(R1)            GET PAST HEADER                              
         AH    R1,DISP             ADD CURRENT DISPLACEMNET                     
         AH    R1,DISLEN           ADD LENGTH OF ENTRY                          
         LR    R0,R2                                                            
         AHI   R0,84               76+8 END OF DISPLAY LINE                     
         CR    R1,R0                                                            
         BH    DR30                SKIP TO NEXT LINE                            
*                                                                               
         SH    R1,DISLEN                                                        
         LH    RE,DISLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,DRMOVE                                                        
         LH    R1,DISP                                                          
         AH    R1,DISLEN                                                        
         AHI   R1,2                                                             
         STH   R1,DISP                                                          
         B     DR35                                                             
*                                                                               
DRMOVE   MVC   0(0,R1),GRPGVAL     EXECUTED                                     
*                                                                               
DR30     DS    0H                                                               
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO NEXT TWA FIELD                       
         LA    R0,SFCPFKH          A(PFKEY INSTRUCTIONS)                        
         CR    R2,R0               ANY MORE FIELDS AVAILABLE?                   
         BNL   DR50                NO                                           
         XC    DISP,DISP                                                        
         B     DR25                                                             
*                                                                               
DR35     GOTO1 SEQ                                                              
         MVC   SFCSTRT(3),GRPGVAL                                               
         B     DR20                                                             
*                                                                               
DR50     MVC   SFCSTRT(3),GRPGVAL  LAST PRODUCT DISPLAYED                       
         OI    SFCSTRTH+6,X'80'                                                 
         OI    6(R2),X'80'         TRANSMIT BIG BLOCK OF TEXT                   
*                                                                               
         MVI   BYTE,C'P'           TELL SUBROUTINE TO DO POSITIONING            
         BAS   RE,CNTPLST          WILL RETURN POSNUMB (FULL WORD)              
*                                                                               
         LA    R1,SFCPFKH                                                       
         LA    R1,51(R1)           MOVE OVER 51 SPACES                          
*                                                                               
         L     RF,POSNUMB          CHECK IF NO MEMBER FOUND                     
         CHI   RF,0                                                             
         BE    DR70C                                                            
         L     RF,NUMBMEM          CHECK IF ZERO MEMBER IN LIST                 
         CHI   RF,0                                                             
         BNE   DR70E                                                            
DR70C    MVI   0(R1),C'0'                                                       
         LA    R1,2(R1)            MOVE OVER 2 SPACES                           
         MVC   0(6,R1),=C'member'                                               
         LA    R1,7(R1)            MOVE OVER 7 SPACES                           
         B     DR70P                                                            
*                                                                               
DR70E    L     R6,POSNUMB                                                       
         AHI   R6,9                                                             
         EDIT  (B4,POSNUMB),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,      +        
               ALIGN=LEFT                                                       
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         MVI   0(R1),C'-'                                                       
         LA    R1,1(R1)            MOVE OVER A SPACE                            
         EDIT  (R6),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,ALIGN=LEFT             
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         LA    R1,1(R1)            MOVE OVER A SPACE                            
         MVC   0(2,R1),=C'of'                                                   
         LA    R1,3(R1)            MOVE OVER 3 SPACES                           
         EDIT  (B4,NUMBMEM),(4,00(R1)),0,COMMAS=YES,ZERO=NOBLANK,      +        
               ALIGN=LEFT                                                       
         AR    R1,R0               ADD NUMBER OF SIGNIFICANT NUMBS              
         LA    R1,1(R1)            MOVE OVER A SPACE                            
DR70P    MVC   0(9,R1),=C'displayed'                                            
*                                                                               
DRLACTV  DS    0H                  DISPLAY ACTIVITY                             
*                                                                               
         MVI   ELCODE,X'F1'        SET ACTIVITY ELEMENT CODE                    
         L     R6,AIO              POINT TO RECORD                              
         BRAS  RE,GETEL            FIND ACTIVITY ELEMENT                        
         BNE   DRLACTVX            SKIP IF NONE FOUND                           
*                                                                               
         MVC   SFCLUPD,=CL13'Last Updated:'                                     
         OI    SFCLUPDH+6,X'80'    TRANSMIT FLD                                 
*                                                                               
         USING ACTVD,R6            ESTABLISH ACTIVITY ELEMENT                   
*                                                                               
         LA    R3,ACTVCHDT         ASSUME LAST CHANGED DATE                     
         OC    ACTVCHDT,ACTVCHDT   IF NO LAST CHANGED DATE                      
         BNZ   *+8                                                              
         LA    R3,ACTVADDT            USE ADDED DATE                            
*                                                                               
         OC    0(3,R3),0(R3)       SKIP IF NO DATE AVAILABLE                    
         BZ    DRLACTVX                                                         
*                                                                               
         GOTOR DATCON,DMCB,(3,0(R3)),(17,SFCDTEU) DISP DATE                     
         OI    SFCDTEUH+6,X'80'    TRANSMIT FLD                                 
*                                                                               
         LA    R2,SFCPIDUH         POINT TO LAST CHANGED FIELD                  
*                                                                               
         LA    R3,ACTVCHID         ASSUME LAST CHANGED PID                      
         OC    ACTVCHID,ACTVCHID   IF NO LAST CHANGED PID                       
         BNZ   *+8                                                              
         LA    R3,ACTVADID            USE ADDED PID                             
*                                                                               
         GOTOR TRNPID,DMCB,0(R3)   DISPLAY PID                                  
*                                                                               
DRLACTVX DS    0H                                                               
*                                                                               
DR70U    OI    SFCPFKH+6,X'80'     TRANSMIT FLD                                 
*                                                                               
         LA    R2,CONACTH          POSITION CURSOR AT ACTION FLD                
         MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(35),=C'Items displayed, hit ENTER for more'              
         CLI   THISLSEL,C'C'       CHANGE ON LIST?                              
         BE    DR78                                                             
         CLI   MODE,VALREC         MODE IS VALREC?                              
         BE    DR78                                                             
         CLI   ACTNUM,ACTCHA       ACTION IS CHANGE?                            
         BNE   DR85                                                             
DR78     MVC   CONHEAD+35(17),=C' or enter changes'                             
         LA    R2,SFCNM1H          POINT TO FIRST FIELD CAN BE CHANGED          
         B     DR85                                                             
*                                                                               
DR80     DS    0H                                                               
         OI    6(R2),X'80'         TRANSMIT BIG BLOCK OF TEXT                   
         OI    SFCSTRTH+6,X'80'                                                 
         LA    R2,CONACTH          POSITION CURSOR AT ACTION FLD                
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(19),=C'All items displayed'                              
         CLI   THISLSEL,C'C'       CHANGE ON LIST?                              
         BE    DR83                                                             
         CLI   MODE,VALREC         MODE IS VALREC?                              
         BE    DR83                                                             
         CLI   ACTNUM,ACTCHA       ACTION IS CHANGE?                            
         BNE   DR85                                                             
DR83     MVC   CONHEAD+19(15),=C', enter changes'                               
         LA    R2,SFCNM1H          POINT TO FIRST FIELD CAN BE CHANGED          
*                                                                               
DR85     OI    CONHEADH+6,X'80'                                                 
         OI    GENSTAT2,USMYOK                                                  
*                                                                               
         CLI   VRSW,C'Y'           RECORD HAS BEEN MODIFIED?                    
         BNE   DR99                                                             
         MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD+00(23),=C'Record has been changed'                       
         MVC   CONHEAD+23(23),=C', hit ENTER to continue'                       
*                                                                               
         CLI   ACTNUM,ACTSEL       SELECT?                                      
         BE    DR87                                                             
         CLI   THISLSEL,C'C'                                                    
         BNE   *+10                                                             
DR87     MVC   CONHEAD+35(19),=C'or PF12 to continue'                           
         OI    CONHEADH+6,X'80'                                                 
         LA    R2,SFCNM1H          POINT TO FIRST FIELD CAN BE CHANGED          
*                                                                               
DR99     B     GOERREX2                                                         
*                                                                               
DRX      J     JUMPXIT1                                                         
         DROP  R4                                                               
*                                                                               
POSDSPSW DS    X                   POSITION DISPLAYING SWITCH                   
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        DISPLAY KEY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                                                               
         L     R4,AIO                                                           
         USING GRPKEY,R4                                                        
         OC    GRPKCODE(2),GRPKCODE                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   DKSVKEY,GRPKEY                                                   
         DROP  R4                                                               
*                                                                               
         MVC   SFCMED(1),DKSVKEY+2                                              
         LA    R2,SFCMEDH                                                       
         MVI   SFCMEDH+5,1                                                      
         OI    SFCMEDH+6,X'80'                                                  
         GOTO1 VALIMED                                                          
*                                                                               
         MVC   SFCCLT(3),DKSVKEY+4                                              
         LA    R2,SFCCLTH                                                       
         MVI   SFCCLTH+5,3                                                      
         OI    SFCCLTH+6,X'80'                                                  
         GOTO1 VALICLT             MUST DO THIS, TO CHECK LIMIT ACCESS          
*                                                                               
         MVC   KEY,DKSVKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                RECORD NOT RESTORED, BAD                     
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING GRPKEY,R4                                                        
         OC    GRPKCODE(2),GRPKCODE                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SFCID,GRPKID                                                     
         MVC   GRPID,SFCID                                                      
         OI    SFCIDH+6,X'80'                                                   
         OI    SFCSTRTH+6,X'80'                                                 
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(8),0(R4)                                                     
         GOTO1 HIGH                GET PRD DEF RECORD                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GRPBRKD,R6                                                       
         MVC   BREAK1LN,GRPBK1LN   SAVE BREAK LENGTHS                           
         MVC   BREAK2LN,GRPBK2LN                                                
*                                                                               
         MVC   SFCCODE,SPACES      CLEAR FIELD                                  
         MVC   GRPCODE,GRPKCODE                                                 
         ICM   R1,B'1100',GRPKCODE                                              
         SRL   R1,12               DD DD ?? ??  =>  00 0D DD D?                 
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'        00 0D DD DS                                  
         UNPK  CODECHAR(5),FULL+1(3)            =>  Z0 ZD ZD ZD ZD              
         ZIC   R1,BREAK1LN         L'BREAK CODES                                
         ZIC   R0,BREAK2LN                                                      
         AR    R1,R0               L'WHOLE GROUP CODE                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SFCCODE(0),CODECHAR+1                                            
         OI    SFCCODEH+6,X'80'                                                 
*                                                                               
         MVC   SFCBK1(L'SFCBK1),SPACES                                          
         MVC   SFCBK1(L'SFCBK1),GRPBK1                                          
         OI    SFCBK1H+6,X'80'                                                  
         MVC   SFCBK1L(L'SFCBK1L),SPACES                                        
         MVI   SFCBK1L,C'('                                                     
         MVC   SFCBK1L+1(1),GRPBK1LN                                            
         OI    SFCBK1L+1,X'F0'                                                  
         MVI   SFCBK1L+2,C')'                                                   
         OI    SFCBK1LH+6,X'80'                                                 
*                                                                               
         MVC   SFCBK2(L'SFCBK2),SPACES                                          
         OI    SFCBK2H+6,X'80'                                                  
         MVC   SFCBK2L(L'SFCBK2L),SPACES                                        
         OI    SFCBK2LH+6,X'80'                                                 
         CLI   GRPBK2LN,0          BREAK 2 MAY NOT BE THERE                     
         BE    DK90                                                             
         MVC   SFCBK2(L'SFCBK2),GRPBK2                                          
         OI    SFCBK2H+6,X'80'                                                  
         MVI   SFCBK2L,C'('                                                     
         MVC   SFCBK2L+1(1),GRPBK2LN                                            
         OI    SFCBK2L+1,X'F0'                                                  
         MVI   SFCBK2L+2,C')'                                                   
         OI    SFCBK2LH+6,X'80'                                                 
*                                                                               
DK90     DS    0H                                                               
         MVC   AIO,AIO1           RESET AIO                                     
         MVC   WKKEY,DKSVKEY      FOR VR USE                                    
*                                                                               
DKX      J     JUMPXIT1                                                         
*                                                                               
DKSVKEY  DS    CL32               SAVING KEY FOR DK USES                        
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        LIST RECORDS                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR       DS    0H                                                               
         BAS   RE,INITBKNM         INIT BREAK NAME/LEN FLDS AND XMIT            
*                                                                               
         LA    R4,KEY                                                           
         USING GRPKEY,R4                                                        
         CLI   GRPKRCOD,GRPKPTYQ   CHECK IF NEED TO BUILD KEY                   
         BE    LR20                                                             
*                                                                               
LR10     DS    0H                  BUILD KEY                                    
         MVC   GRPKAGY,AGENCY                                                   
         MVC   GRPKMED,FLTRMED                                                  
*                                                                               
         CLI   GRPKMED,0                                                        
         BNE   *+8                                                              
         MVI   GRPKMED,C'B'        First media is mobile                        
*                                                                               
         MVI   GRPKRCOD,GRPKPTYQ                                                
         MVC   GRPKCLT,FLTRCLT                                                  
         MVC   GRPKID,FLTRID       ID/CODE                                      
         MVC   GRPKCODE,STRTCODE   GROUP                                        
*                                                                               
LR20     GOTO1 HIGH                                                             
         CLI   KEYSAVE+3,X'FF'     SEE IF SKIP READING MEDIA                    
         BNE   LR30                                                             
         MVI   GRPKRCOD,GRPKPTYQ   MUST GET RIGHT RECORD TYPE AGAIN             
         B     LR20                                                             
*                                                                               
LR30     CLC   GRPKAGY,AGENCY      SAME AGENCY?                                 
         BNE   LRX                                                              
         CLI   GRPKRCOD,GRPKPTYQ   MUST BE PRODUCT GRP RECS                     
         BNE   LR85                NEXT MEDIA                                   
*                                                                               
         CLI   FLTRMED,0                                                        
         BE    *+14                                                             
         CLC   GRPKMED,FLTRMED     FILTERING ON MEDIA?                          
         BNE   LR80                                                             
         OC    FLTRCLT,FLTRCLT                                                  
         BZ    *+14                                                             
         CLC   GRPKCLT,FLTRCLT     FILTERING ON CLIENT?                         
         BNE   LR80                                                             
         CLI   FLTRID,0                                                         
         BE    *+14                                                             
         CLC   GRPKID,FLTRID       FILTERING ON MEDIA?                          
         BNE   LR80                                                             
         CLC   GRPKCODE,STRTCODE   STARTING CODE?                               
         BL    LR80                                                             
*                                                                               
         OC    GRPKCODE,GRPKCODE                                                
         BZ    LR80                                                             
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(8),SAVEKEY      UP TO GRP ID PORTION OF KEY                  
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                ERROR, RECORD NOT FOUND                      
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GRPBRKD,R6                                                       
         MVC   BREAK1LN,GRPBK1LN   GET BREAK LENGTHS                            
         MVC   BREAK2LN,GRPBK2LN                                                
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                RESTORE READING SEQUENCE                     
         CLC   KEY(L'KEY),KEYSAVE                                               
         BE    *+6                                                              
         DC    H'0'                ERROR, WRONG RECORD READ                     
*                                                                               
         MVC   LSTMED(LSTLNQ),SPACES                                            
*                                                                               
         ICM   R1,B'1100',GRPKCODE                                              
         SRL   R1,12               DD DD ?? ??  =>  00 0D DD D?                 
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'        00 0D DD DS                                  
         UNPK  CODECHAR(5),FULL+1(3)            =>  Z0 ZD ZD ZD ZD              
         ZIC   R1,BREAK1LN         L'BREAK CODES                                
         ZIC   R0,BREAK2LN                                                      
         AR    R1,R0               L'WHOLE GROUP CODE                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSTCODE(0),CODECHAR+1                                            
*                                                                               
         MVC   AIO,AIO1            GET RECORD INTO AIO1                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING GRPGRPD,R6                                                       
         MVI   ELCODE,GRPGRPCQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LSTMED,GRPKMED                                                   
         MVC   LSTCLT,GRPKCLT                                                   
         MVC   LSTID,GRPKID                                                     
*                                                                               
         MVC   LSTNAME1(L'LSTNAME1),GRPGNAM1                                    
         OC    GRPGNAM2,GRPGNAM2                                                
         BZ    *+10                                                             
         MVC   LSTNAME2(L'LSTNAME2),GRPGNAM2                                    
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR80                                                             
*                                                                               
LR80     GOTO1 SEQ                 NEXT RECORD                                  
         B     LR30                                                             
*                                                                               
LR85     MVI   KEY+3,X'FF'                                                      
         XC    KEY+4(28),KEY+4                                                  
         B     LR20                SKIP TO NEXT MEDIA                           
*                                                                               
LRX      J     JUMPXIT1                                                         
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        PRINT RECORDS                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       DS    0H                                                               
         TM    WHEN,X'40'          NOW REPORT?                                  
         BZ    PR0                                                              
         CLI   SFCCLTH+5,0         CLIENT FLD PRESENT?                          
         BE    *+12                                                             
         CLI   SFCIDH+5,0          ID FLD PRESENT?                              
         BNE   PR0                                                              
         MVI   ERROR,MISSING                                                    
         LA    R2,SFCIDH                                                        
         J     TRAPERR             NOW REPORT NEED BOTH CLT AND ID FLDS         
*                                                                               
PR0      LA    R3,HEADING          SET UP REPORT HEADING                        
         ST    R3,SPECS                                                         
         LA    R3,HDHK                                                          
         ST    R3,HEADHOOK                                                      
*                                                                               
PR1      XC    SVKEY,SVKEY                                                      
         LA    R4,SVKEY            CLEAR KEY                                    
         USING GRPKEY,R4                                                        
         MVI   GRPKRCOD,GRPKPTYQ                                                
         MVC   GRPKAGY,AGENCY                                                   
         MVC   GRPKMED,QMED                                                     
         MVC   GRPKCLT,QCLT                                                     
         MVC   GRPKID,GRPID                                                     
*                                                                               
         MVC   KEY(25),SVKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(7),SVKEY      CHECK THROUGH CLIENT                           
         BNE   PRX                                                              
         CLI   SFCIDH+5,0         SEE IF ID ENTERED                             
         BNE   PR1D                                                             
         MVC   GRPID,KEY+7                                                      
         B     PR1F                                                             
*                                                                               
PR1D     CLC   KEY(8),SVKEY       IF ID WAS ENTERED -REPORT ONLY IT             
         BNE   PRX                                                              
*                                                                               
PR1F     DS    0H                                                               
         MVC   AIO,AIO2            READ GROUP DEFINITION RECORD                 
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING GRPBRKD,R6                                                       
         MVC   BREAK1LN,GRPBK1LN   SAVE BREAK LENGTHS                           
         MVC   BREAK2LN,GRPBK2LN                                                
         MVC   SH7,SPACES           CLEAR SAVE AREA                             
         MVC   SH7+1(4),=C'CODE'                                                
         MVC   SH7+6(12),GRPBK1     DISPLAY BREAK NAMES AND LENGTHS             
         MVI   SH7+19,C'('                                                      
         MVC   SH7+20(1),GRPBK1LN                                               
         OI    SH7+20,X'F0'                                                     
         MVI   SH7+21,C')'                                                      
         CLI   GRPBK2LN,0          BREAK 2 MAY NOT BE THERE                     
         BE    PR2                                                              
         MVC   SH7+31(12),GRPBK2                                                
         MVI   SH7+44,C'('                                                      
         MVC   SH7+45(1),GRPBK2LN                                               
         OI    SH7+45,X'F0'                                                     
         MVI   SH7+46,C')'                                                      
         DROP  R6                                                               
PR2      MVC   SH7+56(8),=C'PRODUCTS'                                           
         OC    SH7,SPACES                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPKEY,R4                    GROUP RECORD KEY                    
         MVC   GRPKAGY,AGENCY                                                   
         MVC   GRPKMED,QMED                                                     
         MVC   GRPKCLT,QCLT                                                     
         MVC   GRPKID,GRPID                                                     
         MVC   GRPKCODE,=X'0001'   FIRST GROUP RECORD                           
         MVI   GRPKRCOD,GRPKPTYQ                                                
         MVC   SAVEKEY,GRPKEY                                                   
         MVI   GOTU,C'N'                                                        
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
PRGR     CLC   GRPKEY(8),SAVEKEY   PART OF SAME GROUP ID                        
         BE    PR4                                                              
         CLI   GOTU,C'Y'           GOT ONE TO UNDERLINE?                        
         BNE   PR3                                                              
         OC    ABOX,ABOX                                                        
         BZ    PR3                                                              
         L     R3,ABOX             A(BOX DSECT)                                 
         USING BOXD,R3                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'          BOTTOM                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R3                                                               
*                                                                               
PR3      CLC   GRPKEY(7),SAVEKEY     CHECK THROUGH CLT                          
         BNE   PRX                                                              
         TM    WHEN,X'40'           'NOW' REPORT                                
         BO    PRX                  ONLY ONE ID                                 
         CLI   SFCIDH+5,0           SEE IF ID WAS ENTERED                       
         BNE   PRX                  IF SO THEN DONE                             
*                                                                               
         MVC   GRPID,GRPKID                                                     
         MVI   FORCEHED,C'Y'                                                    
         B     PR1                                                              
PR4      MVC   SAVEKEY,GRPKEY                                                   
*                                                                               
         CLI   GOTU,C'Y'                                                        
         BNE   PR7                                                              
         CLI   LINE,55                                                          
         BL    PRM                                                              
         OC    ABOX,ABOX                                                        
         BZ    PR7                                                              
         L     R3,ABOX             A(BOX DSECT)                                 
         USING BOXD,R3                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)    PRINT HORIZONTAL LINE                        
         MVI   0(R1),C'B'          BOTTOM                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R3                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     PR7                                                              
*                                                                               
PRM      OC    ABOX,ABOX                                                        
         BZ    PR7                                                              
         L     R3,ABOX             A(BOX DSECT)                                 
         USING BOXD,R3                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)    PRINT HORIZONTAL LINE                        
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R3                                                               
*                                                                               
PR7      MVI   GOTU,C'Y'                                                        
         GOTO1 GETREC                                                           
*                                                                               
         MVC   GRPCODE,GRPKCODE       FOR CORRECT GRP CODE LOOK UP              
*                                                                               
         ICM   R1,B'1100',GRPKCODE                                              
         SRL   R1,12                  DD DD ?? ??  =>  00 0D DD D?              
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'           00 0D DD DS                               
         UNPK  CODECHAR(5),FULL+1(3)               =>  Z0 ZD ZD ZD ZD           
         ZIC   R1,BREAK1LN            L'BREAK CODES                             
         ZIC   R0,BREAK2LN                                                      
         AR    R1,R0                  L'WHOLE GROUP CODE                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P1+1(0),CODECHAR+1     CODE TO SCREEN LINE BLANK PADDED          
*                                                                               
         L     R6,AIO                                                           
         USING GRPGRPD,R6                                                       
         MVI   ELCODE,GRPGRPCQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P1+6(24),GRPGNAM1  GROUP NAMES TO SCREEN                         
         OC    GRPGNAM2,GRPGNAM2                                                
         BZ    *+10                                                             
         MVC   P1+31(24),GRPGNAM2                                               
         OC    P1,SPACES                                                        
         MVC   SVNMS,P1                                                         
         DROP  R6                                                               
*                                                                               
         MVI   DFLG,C'Y'                                                        
         LA    R2,P1+56                                                         
         MVI   CONT,C'N'                                                        
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPGKEY,R4                                                       
         MVI   GRPGTYP,GRPGPGQ                                                  
         MVC   GRPGAGY,AGENCY                                                   
         MVC   GRPGMED,QMED                                                     
         MVC   GRPGCLT,QCLT        LEAVE GRPGVAL EMPTY                          
         MVC   GRPGID,GRPID                                                     
         MVC   GRPGCODE,GRPCODE                                                 
*                                  GRPCODE WAS SET AT PR7                       
         GOTO1 HIGH                                                             
*                                                                               
PR20     DS    0H                                                               
         CLC   KEY(10),KEYSAVE     CHECK SAME GROUP CODE                        
         BE    PR25                                                             
         B     PRS2                                                             
*                                                                               
PR25     MVI   DFLG,C'Y'                                                        
         MVC   0(3,R2),GRPGVAL     YES - JUST PUT PRODUCT CODE                  
*                                                                               
         MVC   DUB(3),GRPGVAL      PRODUCT CODE                                 
         BRAS  RE,PRPRDNM          PRINT PRODUCT NAME                           
         LA    R0,P1+100                                                        
         LA    R2,30(R2)                                                        
         CR    R2,R0               ANY MORE FIELDS AVAILABLE?                   
         BL    NXM                 YES                                          
*                                                                               
         MVI   CONT,C'Y'                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   DFLG,C'N'                                                        
         LA    R2,P1+56                                                         
*                                                                               
NXM      GOTO1 SEQ                                                              
         B     PR20                                                             
*                                                                               
PRS2     CLI   DFLG,C'Y'                                                        
         BNE   PSQ                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   DFLG,C'N'                                                        
*                                                                               
PSQ      XC    KEY,KEY                                                          
         MVC   KEY(10),SAVEKEY                                                  
         SR    R1,R1                                                            
         ICM   R1,B'0011',KEY+8                                                 
         LA    R1,1(R1)                                                         
         STCM  R1,B'0011',KEY+8                                                 
         GOTO1 HIGH                                                             
         B     PRGR                                                             
*                                                                               
PRX      J     JUMPXIT1                                                         
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        HEADER ROUTINE                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
HDHK     NTR1                                                                   
         MVC   H1+10(1),QMED                                                    
         MVC   H1+15(10),MEDNM                                                  
         OC    H1+10(10),SPACES                                                 
         MVC   H2(6),=C'CLIENT'                                                 
         MVC   H2+10(3),QCLT                                                    
         MVC   H2+15(20),CLTNM                                                  
         MVC   H2+60(2),=C'ID'                                                  
         MVC   H2+63(1),GRPID                                                   
         MVC   H1+52(21),=C'PRODUCT GROUP RECORDS'                              
         MVC   H3+52(21),=C'--------------------'                               
         MVI   H5,0                                                             
         MVC   H7,SH7                                                           
         CLI   CONT,C'Y'                                                        
         BNE   BX                                                               
******** MVC   H7+65(11),=C'(CONTINUED)'  *** UNTIL BUG IS FIXED                
         MVC   P1(55),SVNMS                                                     
*                                                                               
BX       OC    ABOX,ABOX                                                        
         BZ    HDHKX                                                            
         L     R3,ABOX             A(BOX DSECT)                                 
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+5,C'C'                                                   
         MVI   BOXCOLS+30,C'C'                                                  
         MVI   BOXCOLS+55,C'C'                                                  
         MVI   BOXCOLS+131,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R3                                                               
HDHKX    J     JUMPXIT1                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
HEADING  SSPEC H1,1,C'MEDIA'                                                    
         SSPEC H2,1,CLIENT                                                      
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H3,95,RUN                                                        
         SSPEC H4,95,REPORT                                                     
         SSPEC H4,112,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        ROUTINE COUNTING NUMBER OF PRODUCT LIST MEMBERS                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CNTPLST  NTR1                      KEY IS NOT SAVED                             
         CLI   BYTE,C'P'           DOING POSITION CALCULATION?                  
         BE    *+10                                                             
         XC    NUMBMEM,NUMBMEM     CLEAR COUNTER                                
         XC    POSNUMB,POSNUMB     CLEAR POSTION NUMBER COUNTER                 
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING GRPGKEY,RE                                                       
         MVC   GRPGAGY,AGENCY                                                   
         MVC   GRPGMED,QMED                                                     
         MVI   GRPGTYP,GRPGPGQ                                                  
         MVC   GRPGCLT,QCLT                                                     
         MVC   GRPGID,GRPID                                                     
         MVC   GRPGCODE,GRPCODE                                                 
         DROP  RE                                                               
         GOTO1 HIGH                                                             
*                                                                               
         B     CNTPL50                                                          
*                                                                               
CNTPL30  GOTO1 SEQ                                                              
*                                                                               
CNTPL50  CLC   KEY(10),KEYSAVE     CHECK THROUGH GROUP ID                       
         BNE   CNTPLX                                                           
         CLI   BYTE,C'P'           DOING POSITION CALCULATIONS?                 
         BNE   CNTPL60                                                          
         CLC   KEY+10(6),POSVALUE                                               
         BH    CNTPLX                                                           
         L     RE,POSNUMB                                                       
         AHI   RE,1                                                             
         ST    RE,POSNUMB                                                       
         B     CNTPL30                                                          
CNTPL60  L     RE,NUMBMEM                                                       
         AHI   RE,1                COUNTING MEMBERS FOUND                       
         ST    RE,NUMBMEM                                                       
         B     CNTPL30             NEXT RECORD                                  
*                                                                               
CNTPLX   J     JUMPXIT1                                                         
*                                                                               
NUMBMEM  DS    F                   NUMBER OF MEMBERS COUNTED                    
POSNUMB  DS    F                   POSITION NUMBER OF PUB IN LIST               
POSVALUE DS    XL6                 VALUE THAT NEED TO BE POSTIONED              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITBKNM DS    0H                  INIT BREAK NAME/LEN FLDS AND XMIT            
         MVC   SFLTL1,SPACES                                                    
         MVC   SFLTL1+00(03),=C'Sel'                                            
         MVC   SFLTL1+05(02),=C'Md'                                             
         MVC   SFLTL1+09(03),=C'Clt'                                            
         MVC   SFLTL1+14(02),=C'Id'                                             
         MVC   SFLTL1+18(04),=C'Code'                                           
         OI    SFLTL1H+6,X'80'                                                  
*                                                                               
         MVC   SFLTL2,SPACES                                                    
         CLC   SVBK1NM(SVBKLQ),SPACES                                           
         BE    INITBN50                                                         
         MVC   SFLTL2+00(L'SVBK1NM),SVBK1NM                                     
         MVI   SFLTL2+00+L'SVBK1NM+2,C'('                                       
         MVC   SFLTL2+00+L'SVBK1NM+3(1),SVBK1L                                  
         OI    SFLTL2+00+L'SVBK1NM+3,X'F0'                                      
         MVI   SFLTL2+00+L'SVBK1NM+4,C')'                                       
*                                                                               
         CLI   SVBK2L,0            BREAK 2 MAY NOT BE THERE                     
         BE    INITBN60                                                         
         MVC   SFLTL2+26(L'SVBK2NM),SVBK2NM                                     
         MVI   SFLTL2+26+L'SVBK2NM+2,C'('                                       
         MVC   SFLTL2+26+L'SVBK2NM+3(1),SVBK2L                                  
         OI    SFLTL2+26+L'SVBK2NM+3,X'F0'                                      
         MVI   SFLTL2+26+L'SVBK2NM+4,C')'                                       
         B     INITBN60                                                         
*                                                                               
INITBN50 MVC   SFLTL2+00(11),=C'Break Name1'                                    
         MVC   SFLTL2+26(11),=C'Break Name2'                                    
INITBN60 OI    SFLTL2H+6,X'80'                                                  
*                                                                               
INITBNX  BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
RELO     DS    F                                                                
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
BADNUMDG XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADNUMDM),BADNUMDM                                     
GOERREX2 GOTO1 ERREX2                                                           
BADNUMDM DC    C'* ERROR * NUMBER OF DIGITS MUST EQUAL SUM OF BREAK LEN+        
               GTHS'                                                            
*                                                                               
BADGRPCD XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADGRPCM),BADGRPCM                                     
         B     GOERREX2                                                         
BADGRPCM DC    C'* ERROR * GROUP CODE MUST BE NON-ZERO'                         
*                                                                               
NOBREAK2 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOBREAKM),NOBREAKM                                     
         B     GOERREX2                                                         
NOBREAKM DC    C'* ERROR * BREAK 2 IS NOT DEFINED'                              
*                                                                               
NODELETE XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODELETM),NODELETM                                     
         B     GOERREX2                                                         
NODELETM DC    C'* ERROR * NOT A MEMBER OF THIS GROUP'                          
*                                                                               
NOADD    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOADDM),NOADDM                                         
         B     GOERREX2                                                         
NOADDM   DC    C'* ERROR * ALREADY A MEMBER OF THIS GROUP'                      
*                                                                               
NODUP    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NODUPM),NODUPM                                         
         B     GOERREX2                                                         
NODUPM   DC    C'* ERROR * CANNOT HAVE DUPLICATES'                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRPRDNM  NTR1  BASE=*,LABEL=*      PRINT PRODUCT NAME                           
*                                                                               
         MVC   0(3,R2),DUB         PRODUCT CODE                                 
         MVI   03(R2),C' '                                                      
*                                                                               
         MVC   TMPWRK(L'KEY),KEY                                                
         MVC   TMPWRK+L'KEY(L'KEYSAVE),KEYSAVE                                  
*                                                                               
         XC    KEY,KEY             BUILD KEY TO LOOK UP CLIENT REC              
         MVC   KEY+00(02),AGENCY                                                
         MVC   KEY+02(01),QMED                                                  
         MVI   KEY+03,X'06'        PRODUCT RECORD CODE                          
         MVC   KEY+04(03),QCLT     CLIENT CODE                                  
         MVC   KEY+07(03),DUB      PRODUCT CODE                                 
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     REC FOUND?                                   
         BE    *+14                                                             
         MVC   04(20,R2),=C'Error: prd not found'                               
         B     PR20G                                                            
         MVC   AIO,AIO3            USE AIO3 FOR CLEAN WORKING IO AREA           
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         LA    RE,33(RE)                                                        
         CLI   0(RE),X'06'         CHKING FOR PUBNAME ELEM                      
         BE    *+6                                                              
         DC    H'0'                THIS ELEM IS ALWAYS THERE!                   
         MVC   06(20,R2),02(RE)    CLIENT NAME                                  
*                                                                               
PR20G    MVC   KEY,TMPWRK          RESTORE KEYS, DONE USING SH7                 
         MVC   KEYSAVE,TMPWRK+L'KEY                                             
         LA    R4,KEY              R4'S USING GOT RESTORED                      
         MVC   AIO,AIO1            PUT AIO1 BACK TO CURRENT AIO                 
         GOTO1 HIGH                RESTORE SEQUENCES                            
*                                                                               
         J     JUMPXIT1                                                         
         LTORG                                                                  
         EJECT                                                                  
         TITLE 'PRSFM26 - CLIENT GROUP MAINTENANCE - CHKSEC'                    
***********************************************************************         
*                                                                     *         
*        CHECKS TO SEE IF USER CAN MAKE CHANGES TO GROUP RECORD       *         
*                                                                     *         
*NTRY   SVSECELM - CONTAINS SECURITY ELEMENT                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CHKSEC   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         LA    R6,SVSECELM         ESTABLISH SECURITY ELEMENT                   
         USING GRPSECD,R6                                                       
*                                                                               
         OC    SVSECELM,SVSECELM   SKIP IF NO ELEMENT FOUND                     
         BZ    CHKSECX                                                          
*                                                                               
         OC    GRPSPIDS,GRPSPIDS   SKIP IF NO PIDS IF LIST                      
         BZ    CHKSECX                                                          
*                                                                               
         LA    R3,GRPSPID          POINT TO FIRST ADMINISTRATOR PID             
         LA    R0,6                MAX 6 PIDS                                   
*                                                                               
CHKSECLP DS    0H                                                               
*                                                                               
         CLC   SVSFMPID,0(R3)      CHECK IF USER IN LIST                        
         BE    CHKSECFD                                                         
*                                                                               
CHKSECCN DS    0H                                                               
*                                                                               
         LA    R3,L'GRPSPID(R3)    BUMP TO NEXT PID                             
         BCT   R0,CHKSECLP         CHECK NEXT PID                               
*                                                                               
CHKSECDN DS    0H                                                               
*                                  USER NOT IN LIST                             
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         GOTOR ERREX                                                            
*                                                                               
CHKSECFD DS    0H                  USER IS IN LIST - OKAY                       
*                                                                               
CHKSECX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM25 - PRINT WEB IO CONTROLLER - CLRFLD'                     
***********************************************************************         
*                                                                     *         
*        CLEARS A FIELD ON SCREEN AND FORCES RE-TRANSMITTAL           *         
*                                                                     *         
*NTRY   R2==>   FIELD ON SCREEN                                       *         
*                                                                     *         
*EXIT    FIELD CLEARED TO NULLS                                       *         
*        FIELD SET TO BE RE-TRANSMITTED                               *         
*        OUTPUT DATA LENGTH SET TO MAXIMUM                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLRFLD   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD ON SCREEN                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         STC   RF,FLDOLEN          SET MAX OUTPUT LENGTH                        
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR FIELD                                  
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
CLRFLDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PRSFM25 - PRINT WEB IO CONTROLLER - BUMP'                       
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUMP TO NEXT FIELD ON SCREEN                      *         
*                                                                     *         
*              BUMP -  NEXT FIELD                                     *         
*              BUMPU - NEXT UNPROTECTED FIELD                         *         
*                                                                     *         
*              DOES NOT DEPEND ON ADDRESSABILITY                      *         
*                                                                     *         
*NTRY    R2==> CURRENT FIELD                                          *         
*                                                                     *         
*EXIT    R2==> NEXT (UNPROTECTED) FIELD                               *         
*        CC    NEQ - NOT END OF SCREEN                                *         
*              EQ  - END OF SCREEN                                    *         
*                                                                     *         
*                                                                     *         
*NOTE: RF DESTROYED                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUMP     DS    0H                  BUMP TO NEXT FIELD                           
         SR    RF,RF                                                            
         ICM   RF,1,0(R2)          GET LENGTH OF TWA FIELD                      
         AR    R2,RF               POINT TO NEXT FIELD                          
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
*        THIS VERSION BUMPS TO NEXT UNPROTECTED FIELD                           
*                                                                               
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BER   RE                                                               
*                                                                               
         TM    1(R2),X'20'         IF PROTECTED FIELD                           
         JNZ   BUMPU                  GO TO NEXT FIELD                          
*                                                                               
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         TITLE 'PRSFM25 - PRINT WEB IO CONTROLLER - CLRSCRN'                    
***********************************************************************         
*                                                                     *         
*        ROUTINE TO CLEAR FIELDS TO END OF SCREEN                     *         
*                                                                     *         
*              DOES NOT DEPEND ON ADDRESSABILITY                      *         
*                                                                     *         
*NTRY    R2==> STARTING FIELD                                         *         
*                                                                     *         
*EXIT    ALL UNPROTECTED FIELDS ARE CLEARED TO END OF SCREEN          *         
*                                                                     *         
*NOTE: RF DESTROYED                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLRSCRN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
CLRSCRLP DS    0H                                                               
*                                                                               
         BRAS  RE,CLRFLD           CLEAR UNPROTECTED FIELD                      
*                                                                               
CLRSCRCN DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BZ    CLRSCRLP            MORE FIELDS ON SCREEN                        
*                                                                               
CLRSCRDN DS    0H                                                               
*                                                                               
CLRSCRNX DS    0H                  ALL DONE                                     
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C25 - INVOICE COMMENTS MAINT/LIST - TRNPID'                  
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1       A(PID)                                              *         
*                                                                     *         
*EXIT    PUTS PERSONAL ID IN FIRST FIELD                              *         
*        BUMPS TO NEXT FIELD                                          *         
*        PUTS NAME IN THIS FIELD                                      *         
*                                                                     *         
*NOTE    MORE EXTENSIVE THAN ROUTINE IN OTHER MODULES                 *         
*        THEY JUST DISPLAY THE NAME                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TRNPID   NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         L     R5,0(R1)            POINT TO PID                                 
*                                                                               
         OC    0(2,R5),0(R5)       SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE                                         
*                                                                               
         LTR   R2,R2               IF R2 GIVEN                                  
         BZ    *+8                                                              
         BRAS  RE,CLRFLD              INIT OUTPUT                               
*                                                                               
         LA    R4,KEY                                                           
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   CT0KAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   CT0KNUM,0(R5)       SET PID                                      
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
         MVC   AIO,AIO1            RESET IOAREA                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   TPIDNOTF                                                         
*                                                                               
*        FIND USER'S ID                                                         
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TPIDCTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDCTDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         - MATCH ON ELEMENT CODE                      
         BE    TPIDCTFD                                                         
*                                                                               
TPIDCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TPIDCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDCTFD DS    0H                                                               
*                                                                               
*        DISPLAY PERSON ID                                                      
*                                                                               
         MVC   FLDDATA(8),2(RE)    USER'S PID                                   
*                                                                               
         LR    R0,RE               SAVE POINTER                                 
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         LR    RE,R0               RESTORE POINTER                              
*                                                                               
         LA    R3,FLDDATA          USE DATAAREA OF SCREEN FIELD                 
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
*                                                                               
         MVC   SAPEAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   SAPEPID,2(RE)       SET USERID FROM PREVIOUS RECORD              
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
         MVC   AIO,AIO1            RESET IOAREA                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEYSAVE SKIP IF REC NOT FOUND           
         BNE   TPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND NAME ELEMENT                                                      
*                                                                               
TPIDNMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDNMDN                                                         
*                                                                               
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
*                                                                               
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    TPIDNMFD                                                         
*                                                                               
TPIDNMCN DS    0H                                                               
*                                                                               
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDNMLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
TPIDNMDN DS    0H                  NAME ELEMENOT FOUND                          
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDNMFD DS    0H                                                               
*                                                                               
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   TPIDNOTF            NO NAME IN ELEMENT                           
*                                                                               
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         LA    R1,WORK             BUILD NAME IN WORKAREA                       
         XC    WORK,WORK                                                        
*                                                                               
TPIDFMLP DS    0H                  FORMAT PERSON'S NAME                         
*                                                                               
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
*                                                                               
         IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
*                                                                               
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
*                                                                               
TPIDFMCN DS    0H                                                               
*                                                                               
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   TPIDFMDN              END OF ELEMENT REACHED                     
*                                                                               
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         LA    R1,1(R1)            ADD IN A SPACING CHARACTER                   
*                                                                               
         B     TPIDFMLP                                                         
*                                                                               
TPIDFMDN DS    0H                                                               
*                                                                               
         B     TPIDSQSH                                                         
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         MVC   WORK(7),=CL7'UNKNOWN'                                            
         LA    R1,WORK+7           POINT TO NEXT OUTPUT POSITION                
*                                                                               
TPIDSQSH DS    0H                                                               
*                                                                               
         LR    R0,R1               END OF OUTPUT MINUS START                    
         LA    RF,WORK             START OF WORKAREA                            
         SR    R0,RF               EQUALS OUTPUT LENGTH                         
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK,(R0) SQUASH NAME                              
*                                                                               
*        MOVE NAME TO SCREEN                                                    
*                                                                               
         LTR   R2,R2               IF NO SCREEN FIELD GIVEN                     
         BNZ   TPIDSCR                                                          
*                                                                               
         LR    RF,R6                  GET RETURN AREA LENGTH                    
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES         INIT OUT PUT AREA                         
*                                                                               
         L     RF,4(R1)               SAVE SQUASHED LENGTH                      
*                                                                               
         CR    RF,R6                  IF NAME TOO LONG                          
         BNH   *+6                                                              
         LR    RF,R6                     USE MAX FOR RETURN AREA                
*                                                                               
         B     TPIDMVC                                                          
*                                                                               
TPIDSCR  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         STC   RF,FLDOLEN          SET MAX OUTPUT LENGTH                        
*                                                                               
TPIDMVC  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY NAME                                 
*                                                                               
TRNPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T41C25 - INVOICE COMMENTS MAINT/LIST - VALPID'                  
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1       A(PID SAVEAREA)                                     *         
*                                                                     *         
*EXIT    VALIDATES PID IN SCREEN FIELD                                *         
*        BUMPS TO NEXT FIELD                                          *         
*        PUTS NAME IN THIS FIELD                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALPID   NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         L     R5,0(R1)            POINT TO PID SAVEAREA                        
*                                                                               
         CLI   FLDILEN,0           SKIP IF NO PID ENTERED                       
         BZ    VALPIDX                                                          
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
*                                                                               
         MVC   SAPEAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,AGENCY         USE BUYREC'S AGENCY                       
*                                                                               
         MVC   SAPEPID,SPACES      INIT PID                                     
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDILEN          GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SAPEPID(0),FLDDATA  MOVE PID TO KEY                              
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
         MVC   AIO,AIO1            RESET IOAREA                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEYSAVE MUST FIND RECORD                
         BNE   VPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND PASSWORD ELEMENT                                                  
*                                                                               
VPIDPWLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    VPIDPWDN                                                         
*                                                                               
         USING SAPWDD,RE           ESTABLISH AS PWD ELEMENT                     
*                                                                               
         CLI   SAPWDEL,SAPWDELQ    LOOKING FOR PWD ELEMENT                      
         BE    VPIDPWFD                                                         
*                                                                               
VPIDPWCN DS    0H                                                               
*                                                                               
         IC    RF,SAPWDLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     VPIDPWLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
VPIDPWDN DS    0H                  PWD ELEMENT NOT FOUND                        
*                                                                               
         B     VPIDNOTF                                                         
*                                                                               
VPIDPWFD DS    0H                                                               
*                                                                               
         MVC   0(L'SAPWDNUM,R5),SAPWDNUM RETURN PID                             
*                                                                               
VALPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
VPIDNOTF DS    0H                  INVALID PID                                  
*                                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPWIO00 - PRINT WEB IO CONTROLLER - PID'                        
***********************************************************************         
*   PID - THIS ROUTINE WILL GET TWO BYTES FROM FATBLOCK               *         
*         WHICH ARE "PERSONAL ID"                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
PID      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         XC    SVSFMPID,SVSFMPID   PASSWORD ID NUMBER CLEARED                   
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0),0,0   RETURN TIME IN TUS                     
         DROP  RF                                                               
*                                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         MVC   SVSECAGY,FATAGYSC   ALSO NEEDED TO GET CORRECT PID               
*                                                                               
         TM    FATFLAG,X'08'       CHECK IF SECRET CODE IS THERE                
         BZ    *+10                                                             
         MVC   SVSFMPID,FAPASSWD   SAVE PASSWORD ID NUMBER                      
*                                                                               
PIDX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
*                                                                               
         PRINT ON                                                               
       ++INCLUDE DDACTIVD                                                       
*                                                                               
         EJECT                                                                  
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMB4D          MAINT SCREEN                                 
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMBDD          LIST SCREEN                                  
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         ORG   SYSSPARE            WORK AREA                                    
MINIO    DS    V                   A(MINIO)                                     
*                                                                               
GRPID    DS    C                   GROUP ID                                     
GRPCODE  DS    XL2                 GROUP CODE (PWOS)                            
*                                                                               
STRTCODE DS    XL2                 START GROUP CODE FOR LIST                    
FLTRMED  DS    C                   FILTER MEDIA CODE FOR LIST                   
FLTRCLT  DS    CL3                 FILTER CLIENT CODE FOR LIST                  
FLTRID   DS    C                   FILTER ID CODE FOR LIST                      
SFLNQ    EQU   *-STRTCODE          EQ LENGTH, KEEP FLDS TOGETHER                
*                                                                               
SAVEKEY  DS    XL32                                                             
CODECHAR DS    CL5                 GROUP CODE CHARACTER                         
*                                                                               
BREAK1LN DS    X                   BREAK 1 LENGTH FROM DEFINITION REC           
BREAK2LN DS    X                   BREAK 2 LENGTH FROM DEFINITION REC           
BKLNQ    EQU   *-BREAK1LN          EQ LENGTH, KEEP FLDS TOGETHER                
*                                                                               
SVSECAGY DS    CL2                 SECURITY AGENCY SAVEAREA                     
SVSECELM DS    XL256               SECURITY ELEMENT SAVEAREA                    
*                                                                               
VALUES   DS    XL((15*VALUELNQ)+1) SAVED VALUES FROM SCREEN (MAX = 15)          
MYWORK   DS    XL64                                                             
SH7      DS    CL132                                                            
SVNMS    DS    CL55                                                             
DFLG     DS    C                   STUFF TO PRINT                               
CONT     DS    C                   CONTINUATION FLAG                            
GOTU     DS    C                   GOT ONE TO UNDERLINE FLAG                    
*                                                                               
DISP     DS    H                                                                
DISLEN   DS    H                                                                
*                                                                               
SVBK1NM  DS    CL12                BREAK NAMES AND LENGTHS TO BE SET UP         
SVBK1L   DS    C                   FOR LIST AND MAINT SCREENS                   
SVBK2NM  DS    CL12                                                             
SVBK2L   DS    C                                                                
SVBKLQ   EQU   *-SVBK1NM           EQ LENGTH, KEEP FLDS TOGETHER                
*                                                                               
SVNAME1  DS    XL24                SAVE NAME 1 FOR VR USES                      
SVNAME2  DS    XL24                SAVE NAME 2 FOR VR USES                      
*                                                                               
WKKEY    DS    CL32                WORKING STORAGE KEY                          
*                                                                               
MINBLOCK DS    XL(MINBLKL)         MINIO PARAMETER BLOCK                        
*                                                                               
TMPWRK   DS    XL100               TEMP WORKING STORAGE AREA                    
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE PGENGRP                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDMINBLK                                                       
*                                                                               
       ++INCLUDE DDFLDHDR                                                       
*                                                                               
       ++INCLUDE CTGENFILE                                                      
*                                                                               
       ++INCLUDE SEACSFILE                                                      
         EJECT                                                                  
*                                                                               
VALUED   DSECT                                                                  
*                                                                               
VALUEVAL DS    CL(L'GRPPVAL)       VALUE TO BE STORED IN RECORD/POINTER         
VALUEACT DS    C                   USER ACTION (C'+' OR C'-')                   
VALUEDEL DS    XL2                 GRP CODE TO BE DELETED                       
*                                                                               
VALUELNQ EQU   *-VALUED                                                         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTMED   DS    CL1                                                              
         DS    CL3                                                              
LSTCLT   DS    CL3                                                              
         DS    CL2                                                              
LSTID    DS    CL1                                                              
         DS    CL3                                                              
LSTCODE  DS    CL4                                                              
         DS    CL2                                                              
LSTNAME1 DS    CL24                                                             
         DS    CL2                                                              
LSTNAME2 DS    CL24                                                             
LSTLNQ   EQU   *-LSTMED                                                         
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094PRSFM29   07/21/16'                                      
         END                                                                    

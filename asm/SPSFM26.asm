*          DATA SET SPSFM26    AT LEVEL 010 AS OF 03/29/06                      
*PHASE T21726A                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE DLFLD                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T21726 - STATION/CLIENT GROUP MAINT/LIST              *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST  *         
*                                                                     *         
*  INPUTS       SCREEN T217C6 (STATION MAINTENANCE)                   *         
*               SCREEN T217C7 (CLIENT MAINTENANCE)                    *         
*               SCREEN T217B4 (LIST)                                  *         
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
*  I/O AREAS    IO1 - STATION/CLIENT GROUP RECORD                     *         
*               IO2 - GROUP DEFINITION RECORD                         *         
*               IO3 - MINIO RECORD TABLE                              *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21726 - STATION/CLIENT GROUP DEFINITION RECORDS'               
T21726   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21726,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         L     RE,=A(DOWNPRR8)                                                  
         A     RE,RELO             YSFI                                         
         STM   R8,RA,0(RE)                                                      
*                                                                               
INT      BAS   RE,INIT             INITIALIZE                                   
         CLI   ACTNUM,ACTSEL       SELECT?                                      
         BNE   CM                                                               
         TM    GENSTAT2,NEXTSEL                                                 
         BO    XIT                                                              
         CLI   PFAID,12            PF 12 OR 24 FOR RETURN?                      
         BE    RTN                                                              
         CLI   PFAID,24                                                         
         BNE   STY                                                              
RTN      OI    GENSTAT2,NEXTSEL+RETEQSEL                                        
         XC    SFSSTRT,SFSSTRT                                                  
         MVI   PFAID,0                                                          
         B     XIT                                                              
STY      OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
*                                                                               
CM       CLI   ACTNUM,ACTDEL       DELETE NOT ALLOWED                           
         BNE   *+12                                                             
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
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
XIT      XIT1                                                                   
         EJECT                                                                  
INIT     NTR1                                                                   
         OI    CONSERVH+1,X'01'    MODIFY                                       
         OI    CONSERVH+6,X'80'    XMIT                                         
*                                                                               
         MVI   IOOPT,C'Y'          GENCON DOES NO WRITES                        
         MVI   ACTELOPT,C'N'       DON'T WRITE 'F1' ELEMENTS                    
         OI    GENSTAT4,NODELLST   NO DELETIONS FROM LIST                       
         CLI   ACTNUM,ACTLIST      SET UP ADCONS FOR FIELDS CONTAINING          
         BE    INIT10              BREAK NAMES AND LENGTHS                      
*                                                                               
         LA    RF,SFSBK1H          MAINTENANCE SCREEN                           
         ST    RF,ABK1FH                                                        
         LA    RF,SFSBK1LH                                                      
         ST    RF,ABK1LFH                                                       
         LA    RF,SFSBK2H                                                       
         ST    RF,ABK2FH                                                        
         LA    RF,SFSBK2LH                                                      
         ST    RF,ABK2LFH                                                       
         B     INIT20                                                           
*                                                                               
INIT10   LA    RF,SFLBK1H          LIST SCREEN                                  
         ST    RF,ABK1FH                                                        
         LA    RF,SFLBK1LH                                                      
         ST    RF,ABK1LFH                                                       
         LA    RF,SFLBK2H                                                       
         ST    RF,ABK2FH                                                        
         LA    RF,SFLBK2LH                                                      
         ST    RF,ABK2LFH                                                       
*                                                                               
INIT20   L     RE,=V(DLFLD)                                                     
         A     RE,RELO                                                          
         ST    RE,VDLFLD                                                        
*                                                                               
         XC    DMCB(24),DMCB       GET A(MINIO)                                 
         MVC   DMCB+4(4),=X'D9000A74'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   MINIO,DMCB                                                       
*                                                                               
         LA    RE,MINBLOCK         CLEAR MINBLOCK                               
         LA    RF,MINBLKL                                                       
         XCEFL                                                                  
         L     RF,=V(RECUP)                                                     
         A     RF,RELO                                                          
         ST    RF,MINRECUP         A(RECUP)                                     
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=CL8'SPTFIL' FILE NAME                                    
         MVC   MINDIR,=CL8'SPTDIR' DIRECTORY NAME                               
         MVI   MINFKLEN,L'GRPKEY   KEY LENGTH                                   
         MVI   MINNCTL,1           NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=AL2(1976) MAXIMUM RECORD LENGTH                        
         MVI   MINEKLEN,GRPKELQL   ELEMENT KEY LENGTH                           
         MVI   MINEKDSP,GRPKMSQL   ELEMENT KEY DISP                             
         MVC   MINBUFF,ATIA        A(FIRST BUFFER)                              
         MVI   MINNBUF,3           USE THREE BUFFERS                            
         MVC   MINRTAB,AIO3        A(AREA FOR RECORD TABLE)                     
         MVC   MINRTABL,=AL2(1900) LENGTH OF RECORD TABLE                       
         MVI   MINFILTL,1          FILTER LENGTH OF 1                           
         LA    RE,ELEM                                                          
         ST    RE,MINELEM          A(AREA FOR ELEMENT)                          
         MVC   MINMAXEL,=AL2(100)  MAX LENGTH OF ELEMENT                        
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
         DROP  R3                                                               
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       CLI   ACTNUM,ACTREP       REPORT                                       
         BE    VK20                                                             
         CLI   ACTNUM,ACTLIST      NOT FOR LIST                                 
         BE    VK10                ALWAYS VALIDATE FOR LIST                     
*                                                                               
         LA    R2,SFSSTRTH         START STATION/CLIENT FIELD                   
         CLI   5(R2),0                                                          
         BE    VKCC                                                             
         TM    4(R2),X'80'         FIELD INPUT THIS TIME                        
         BNO   VKCC                THEN ASSUME VALID                            
         MVI   ERROR,NOTALPHA                                                   
         TM    4(R2),X'04'         ALPHABETIC CHARACTERS?                       
         BZ    TRAPERR                                                          
*                                                                               
VKCC     CLI   ACTNUM,ACTDIS       ALWAYS VALIDATE FOR DISPLAY                  
         BE    VK10                                                             
         TM    SFSMEDH+4,X'20'     HAS KEY CHANGED?                             
         BZ    VK10                                                             
         TM    SFSIDH+4,X'20'                                                   
         BZ    VK10                YES, VALIDATE                                
         TM    SFSCODEH+4,X'20'                                                 
         BZ    VK10                                                             
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VK60                DON'T MAKE THEM CHANGE ACTION                
         OI    WHENOK,X'01'        SET WHENOK X'01' BIT IN TABLE                
         B     VKX                                                              
*                                                                               
VK10     CLI   ACTNUM,ACTLIST      NOT START FIELD FOR LIST                     
*        BE    *+10                                                             
*        XC    SFSSTRT,SFSSTRT                                                  
         NI    SFSMEDH+4,X'FF'-X'20'  YES -- VALIDATE THE KEY                   
         NI    SFSIDH+4,X'FF'-X'20'                                             
         NI    SFSCODEH+4,X'FF'-X'20'                                           
*                                                                               
         XC    SVKEY2,SVKEY2                                                    
         LA    R4,SVKEY2           CLEAR KEY                                    
         USING GRPKEY,R4                                                        
*                                                                               
         MVI   GRPKTYP,GRPKTYPQ    RECORD TYPE                                  
         MVI   GRPKSTYP,GRPKCTYQ                                                
         CLI   RECNUM,34           CLIENT GROUP?                                
         BE    VK20                                                             
         MVI   GRPKSTYP,GRPKSTYQ                                                
         CLI   RECNUM,31           STATION GROUP?                               
         BE    VK20                                                             
         DC    H'0'                HOW DID WE GET HERE?                         
*                                                                               
VK20     LA    R2,SFSMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         CLI   ACTNUM,ACTREP       REPORT                                       
         BE    VK25                                                             
         MVC   GRPKAGMD,BAGYMD                                                  
*                                                                               
         L     RF,ABK1FH           CLEAR BREAK NAME/LEN FIELDS AND XMIT         
         XC    8(L'SFSBK1,RF),8(RF)                                             
         OI    6(RF),X'80'                                                      
         L     RF,ABK1LFH                                                       
         XC    8(L'SFSBK1L,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
         L     RF,ABK2FH                                                        
         XC    8(L'SFSBK2,RF),8(RF)                                             
         OI    6(RF),X'80'                                                      
         L     RF,ABK2LFH                                                       
         XC    8(L'SFSBK2L,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
*                                                                               
VK25     LA    R2,SFSIDH           GROUP ID                                     
         CLI   ACTNUM,ACTREP       REPORT                                       
         BNE   VKI                                                              
         CLI   5(R2),0             ENTERED                                      
         BE    VB                                                               
         MVI   ERROR,NOTALPHA                                                   
         TM    4(R2),X'04'         ALPHABETIC CHARACTER?                        
         BZ    TRAPERR                                                          
* GET ONE BYTE GROUP ID VALUE                                                   
         MVC   DUB(2),8(R2)                                                     
         OI    DUB+1,C' '                                                       
         BAS   RE,TRANS21                                                       
         MVC   GRPID,2(R1)                                                      
         B     VK60                                                             
*                                                                               
VB       MVI   GRPID,0                                                          
         B     VK60                                                             
*                                                                               
VKI      MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    TRAPERR                                                          
*                                                                               
         MVI   ERROR,NOTALPHA                                                   
         TM    4(R2),X'04'         ALPHABETIC CHARACTER?                        
         BZ    TRAPERR                                                          
* GET ONE BYTE GROUP ID VALUE                                                   
         MVC   DUB(2),8(R2)                                                     
         OI    DUB+1,C' '                                                       
         BAS   RE,TRANS21                                                       
         MVC   GRPID,2(R1)                                                      
*                                                                               
         MVC   GRPKID,GRPID        PUT ID IN KEY                                
         MVC   KEY(13),SVKEY2                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,NOTFOUND                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   TRAPERR                                                          
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
*                                                                               
VGD      MVC   AIO,AIO2            READ GROUP DEFINITION REC INTO IO2           
         GOTO1 GETREC               M                                           
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
         L     RF,ABK1FH           DISPLAY BREAK NAMES AND LENGTHS              
         MVC   8(L'SFSBK1,RF),GRPBK1                                            
         L     RF,ABK1LFH                                                       
         MVI   8(RF),C'('                                                       
         MVC   9(1,RF),GRPBK1LN                                                 
         OI    9(RF),X'F0'                                                      
         MVI   10(RF),C')'                                                      
*                                                                               
         CLI   GRPBK2LN,0          BREAK 2 MAY NOT BE THERE                     
         BE    VK30                                                             
         L     RF,ABK2FH                                                        
         MVC   8(L'SFSBK2,RF),GRPBK2                                            
         L     RF,ABK2LFH                                                       
         MVI   8(RF),C'('                                                       
         MVC   9(1,RF),GRPBK2LN                                                 
         OI    9(RF),X'F0'                                                      
         MVI   10(RF),C')'                                                      
*                                                                               
VK30     MVC   GRPKID,GRPID        PUT ID IN KEY                                
         MVC   AIO,AIO1            RESTORE AIO                                  
         LA    R2,SFSCODEH         CODE                                         
         CLI   5(R2),0                                                          
         BNE   VK40                                                             
         MVI   ERROR,MISSING                                                    
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   TRAPERR                                                          
         MVC   STRTCODE,=X'0001'   SET FOR FIRST GROUP RECORD                   
         B     VK60                                                             
*                                                                               
VK40     MVI   ERROR,NOTNUM                                                     
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    TRAPERR                                                          
         ZIC   R0,BREAK1LN         NO. OF DIGITS MUST EQUAL THE. . .            
         ZIC   R1,BREAK2LN         . . . SUM OF THE BREAK LENGTHS               
         AR    R0,R1                                                            
         CLM   R0,1,5(R2)                                                       
         BNE   BADNUMDG                                                         
         MVC   FULL,8(R2)          GROUP CODES ARE LEFT-JUSTIFIED, PWOS         
         OC    FULL,=C'0000'                                                    
         PACK  DUB,FULL                                                         
         L     R0,DUB+4                                                         
         SRA   R0,4                                                             
         BZ    BADGRPCD            AND MUST BE NON-ZERO                         
         STCM  R0,3,GRPCODE                                                     
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   *+14                                                             
         MVC   STRTCODE,GRPCODE                                                 
         B     VK60                                                             
         MVC   GRPKCODE,GRPCODE                                                 
*                                                                               
VK60     XC    KEY,KEY                                                          
         MVC   KEY(6),SVKEY2                                                    
         MVI   KEY+6,X'FF'                                                      
         MVC   KEY+7(6),KEY+6                                                   
*                                                                               
VKX      B     XIT                                                              
         DROP  R4                                                               
*                                                                               
TRANS21  LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
TRANS21A CLC   DUB(2),0(R1)                                                     
         BER   RE                                                               
         LA    R1,3(R1)                                                         
         BCT   R0,TRANS21A                                                      
         B     BADCODE                                                          
         EJECT                                                                  
* VALIDATE RECORD   * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
*************  NEED TO SEE IF USER HAS ACCESS TO THIS  ****************         
VR       DS    0H                                                               
         MVI   MISCFLG1,X'FF'-MF1UPD   RESET THIS FLAG                          
         CLI   RECNUM,34            WE DOING CGROUP RECORDS?                    
         BNE   VR00X                - NOPE                                      
*                                                                               
         BRAS  RE,USERCHK                                                       
*                                                                               
         BNE   BADUSER                                                          
*************  NEED TO SEE IF USER HAS ACCESS TO THIS  ****************         
*                                                                               
VR00X    LA    R2,SFSNM1H          NAME 1 FIELD                                 
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         LA    R2,SFSNM2H          NAME 2 FIELD                                 
         CLI   BREAK2LN,0                                                       
         BNE   *+16                                                             
         CLI   5(R2),0                                                          
         BE    VR10                                                             
         B     NOBREAK2                                                         
         CLI   5(R2),0                                                          
         BNE   VR10                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR10     DS    0H                                                               
         LA    R2,SFSNEWH          LIST OF STATNS/CLTS TO BE VALIDATED          
*********CLI   5(R2),0             *** REMOVED BY DAVID E.                      
*********BE    DR                                                               
         MVC   SFSSTRT,SFSLIST     DISPLAY SAME ITEMS AGAIN                     
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
         MVI   MYWORK,16+8         HEADER + EXT + 8-BYTE FIELD                  
         CLI   RECNUM,34           CLIENTS?                                     
         BNE   *+8                                                              
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
         CLI   RECNUM,34           CLIENTS?                                     
         BNE   VR30                NO, STATIONS                                 
         GOTO1 VALICLT             VALIDATE THE CLIENT CODE                     
         MVC   VALUEVAL(3),QCLT                                                 
         B     VR40                                                             
VR30     GOTO1 VALISTA             VALIDATE THE STATION CODE                    
         MVC   VALUEVAL(5),QSTA                                                 
*                                                                               
VR40     L     R2,ACURFORC                                                      
         XC    KEY,KEY                                                          
         MVI   GRPPTYP,GRPPTYPQ    BUILD PASSIVE POINTER                        
         MVI   GRPPSTYP,GRPPCTYQ                                                
         CLI   RECNUM,34           CLIENT GROUP?                                
         BE    *+8                                                              
         MVI   GRPPSTYP,GRPPSTYQ   NO, STATION GROUP                            
         MVC   GRPPAGMD,BAGYMD                                                  
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
         CLC   KEY(13),KEYSAVE                                                  
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
*                                                                               
VR60     ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         CLI   0(R2),17            ANY MORE FIELDS?                             
         BH    VR20                YES                                          
         EJECT                                                                  
         LA    R2,SFSNEWH                                                       
         ST    R2,ACURFORC                                                      
*                                                                               
* RECORD HAS NOW BEEN VALIDATED                                                 
*                                                                               
         LA    R6,ELEM                                                          
         MVC   MINMKEY(GRPKMSQL),SVKEY2                                         
*                                                                               
         OC    MINMKEY+4(2),MINMKEY+4    <--NOT SURE HOW WE GET HERE            
         BNZ   VR65                                                             
         MVC   MINMKEY+4(2),GRPCODE                                             
         MVC   SVKEY2,MINMKEY                                                   
*                                                                               
VR65     DS    0H                  ****   MOVED TO VR153   ****                 
*                                                                               
VR80     LA    R3,VALUES           LIST OF SAVED STATIONS/CLIENTS               
*                                                                               
VR90     CLI   0(R3),X'FF'         ANY MORE VALUES TO PROCESS?                  
         BE    VR153               NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   GRPPTYP,GRPPTYPQ    BUILD PASSIVE POINTER                        
         MVI   GRPPSTYP,GRPPCTYQ                                                
         CLI   RECNUM,34           CLIENT GROUP?                                
         BE    *+8                                                              
         MVI   GRPPSTYP,GRPPSTYQ   NO, STATION GROUP                            
         MVC   GRPPAGMD,BAGYMD                                                  
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
         CLC   KEY(13),KEYSAVE     IS THE PASSIVE ALREADY THERE?                
         BE    VR110                                                            
*                                                                               
         CLI   VALUEACT,C'+'       NO -- THIS MUST BE AN ADD                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   MYWORK(18),KEY      SAVE KEY                                     
         XC    KEY+10(2),KEY+10    CLEAR GROUP CODE                             
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(10),KEYSAVE     IS VALUE ASSIGNED SOMEWHERE ELSE?            
         BNE   VR100                                                            
         OI    GRPPCNTL,X'80'      YES -- DELETE THAT POINTER                   
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   VALUEDEL,GRPPCODE   MUST DELETE AN ELEMENT LATER                 
*                                                                               
VR100    XC    KEY,KEY                                                          
         MVC   KEY(13),MYWORK      RESTORE KEY                                  
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
         MVC   MYWORK(18),KEYSAVE  SAVE KEY                                     
         XC    KEY+10(2),KEY+10    CLEAR GROUP CODE                             
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(10),KEYSAVE     IS VALUE ASSIGNED SOMEWHERE ELSE?            
         BNE   VR120                                                            
         OI    GRPPCNTL,X'80'      YES -- DELETE THAT POINTER                   
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   VALUEDEL,GRPPCODE   MUST DELETE AN ELEMENT LATER                 
*                                                                               
VR120    XC    KEY,KEY                                                          
         MVC   KEY(18),MYWORK                                                   
         NI    GRPPCNTL,X'FF'-X'80'                                             
         GOTO1 WRITE               RESTORE PASSIVE POINTER                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR130    XC    ELEM,ELEM           CLEAR MINELEM AREA                           
         USING GRPVALD,R6                                                       
         MVI   GRPVALCD,GRPVALCQ   BUILD VALUE ELEMENT                          
         MVI   GRPVALLN,GRPVALLQ                                                
         MVC   GRPVALUE,VALUEVAL                                                
         DROP  R6                                                               
*                                                                               
         OI    MISCFLG1,MF1UPD     SOMETHING WAS ADDED, UPDATE GRPGRPEL         
         GOTO1 MINIO,DMCB,('MINADD',(R5))  ADD THE ELEMENT                      
         CLI   MINERR,0                                                         
         BE    VR150                                                            
         DC    H'0'                                                             
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
         LA    R6,ELEM                                                          
         MVC   MINMKEY(GRPKMSQL),SVKEY2                                         
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,GRPVALCQ    FIND THE CLIENT/STATION ELEMENT              
         MVC   MINEKEY+1(L'GRPVALUE),SPACES                                     
         CLI   RECNUM,34           CLIENT GROUPS?                               
         BE    *+14                                                             
         MVC   MINEKEY+1(5),VALUEVAL   STATION                                  
         B     *+10                                                             
         MVC   MINEKEY+1(3),VALUEVAL   CLIENT                                   
*                                                                               
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,3            <-- FOR NOW SKIP TOO MANY MESSED UP          
         BE    VR150               WE'RE TRYING TO DELETE                       
         CLI   MINERR,0            AND IT'S NOT THERE SO IT'S COOL              
         BE    *+6                                                              
         DC    H'0'                IT MUST BE THERE                             
*                                                                               
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0            DELETE IT                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    MISCFLG1,MF1UPD     SOMETHING WAS GONE, UPDATE GRPGRPEL          
*                                                                               
VR150    LA    R3,VALUELNQ(R3)     BUMP TO NEXT SAVED STATION/CLIENT            
         B     VR90                                                             
*                                                                               
****  USED TO BE VR65 AND VR70, MOVED DOWN TO ADD FLAG                          
VR153    XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,GRPGRPCQ    GET GROUP NAME ELEMENT                       
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            ANY?                                         
         BE    VR155               YES                                          
         CLI   MINERR,MINESNF      RECORD SET NON-EXISTANT?                     
         BE    VR153G               - CHECK IF IT'S ADDING                      
*                                                                               
         CLI   MINERR,MINEEOF      IT SHOULDN'T FIND IT                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
VR153G   CLI   ACTNUM,ACTADD       ARE WE ADDING?                               
         BE    *+6                  - YUP, IT'S OK                              
         DC    H'0'                                                             
*                                                                               
         USING GRPGRPD,R6                                                       
         XC    ELEM,ELEM           CLEAR MINELEM AREA                           
         MVI   GRPGRPCD,GRPGRPCQ   BUILD GROUP NAME ELEMENT                     
         MVI   GRPGRPLN,GRPGRPLQ                                                
         MVC   GRPGNAM1,SFSNM1                                                  
         MVC   GRPGNAM2,SFSNM2                                                  
         MVC   GRPGRPID,PIDNUM                                                  
         GOTO1 DATCON,DMCB,(5,0),(2,GRPGRDAT)                                   
         GOTO1 MINIO,DMCB,('MINADD',(R5))  ADD THE ELEMENT                      
         CLI   MINERR,0                                                         
         BE    VR160                                                            
         DC    H'0'                                                             
*                                                                               
VR155    TM    MISCFLG1,MF1UPD     WERE STATIONS/CLIENTS UPDATED?               
         BO    VR157                - YUP, THEY WERE                            
         TM    SFSNM1H+4,X'20'     IF NAMES FIELDS CHANGED,                     
         BZ    VR157                                                            
         TM    SFSNM2H+4,X'20'                                                  
         BO    VR160                                                            
*                                                                               
VR157    XC    ELEM,ELEM           THEN CHANGE ELEMENT                          
         MVI   GRPGRPCD,GRPGRPCQ   BUILD GROUP NAME ELEMENT                     
         MVI   GRPGRPLN,GRPGRPLQ                                                
         MVC   GRPGNAM1,SFSNM1                                                  
         MVC   GRPGNAM2,SFSNM2                                                  
         MVC   GRPGRPID,PIDNUM                                                  
         GOTO1 DATCON,DMCB,(5,0),(2,GRPGRDAT)                                   
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
VR160    GOTO1 MINIO,DMCB,('MINCLS',(R5))  CLOSE THE RECORD BUFFER              
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,VALUES           LOOK FOR WRAP-UP DELETIONS                   
*                                                                               
VR170    CLI   0(R3),X'FF'         END OF LIST?                                 
         BE    VR190                                                            
*                                                                               
         OC    VALUEDEL,VALUEDEL   ANYTHING TO DELETE?                          
         BZ    VR180               NO                                           
*                                                                               
         LA    R6,ELEM                                                          
         MVC   MINMKEY(GRPKMSQL),SVKEY2                                         
         MVC   MINMKEY+4(2),VALUEDEL  GROUP CODE OF OFFENDING RECORD            
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,GRPVALCQ    FIND THE CLIENT/STATION ELEMENT              
         MVC   MINEKEY+1(L'GRPVALUE),SPACES                                     
         CLI   RECNUM,34           CLIENT GROUPS?                               
         BE    *+14                                                             
         MVC   MINEKEY+1(5),VALUEVAL   STATION                                  
         B     *+10                                                             
         MVC   MINEKEY+1(3),VALUEVAL   CLIENT                                   
*                                                                               
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,3            <-- FOR NOW SKIP TOO MANY MESSED UP          
         BE    VR175               WE'RE TRYING TO DELETE                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                IT MUST BE THERE                             
*                                                                               
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0            DELETE IT                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR175    GOTO1 MINIO,DMCB,('MINCLS',(R5))  CLOSE THE RECORD BUFFER              
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR180    LA    R3,VALUELNQ(R3)     BUMP TO NEXT VALUE IN LIST                   
         B     VR170                                                            
         DROP  R3                                                               
*                                                                               
VR190    LA    R2,SFSNEWH          CLEAR INPUT FIELDS                           
*                                                                               
VR200    CLI   5(R2),0             IF ANYTHING IS IN FIELD,                     
         BE    VR210                                                            
         ZIC   R1,0(R2)            . . . THEN CLEAR IT                          
         SH    R1,=H'17'           L'HDR + L'EXT + 1 FOR EX                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
VR210    OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),17            ANY MORE FIELDS TO CLEAR?                    
         BH    VR200               MAYBE                                        
*                                                                               
         EJECT                                                                  
* DISPLAY RECORD  * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
DR       LA    R6,ELEM                                                          
         MVC   MINMKEY(GRPKMSQL),SVKEY2                                         
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,GRPGRPCQ    GET GROUP NAME ELEMENT                       
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            ANY?                                         
         BE    DR10                YES                                          
         CLI   ACTNUM,ACTADD       ADD                                          
         BE    *+12                                                             
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
         CLI   MINERR,MINESNF      RECORD SET NON-EXISTENT?                     
         BE    *+6                                                              
         DC    H'0'                DIE ON OTHER ERROR                           
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMSGNO1,2          'PLEASE ENTER FIELDS AS REQUIRED'            
         MVI   GTMSYS,GTGENSYS                                                  
         DROP  RF                                                               
         OI    GENSTAT2,USMYOK+USGETTXT                                         
         LA    RF,SFSNM1H                                                       
         ST    RF,ACURFORC                                                      
         B     XIT                                                              
*                                                                               
         USING GRPGRPD,R6                                                       
DR10     MVC   SFSNM1,GRPGNAM1     DISPLAY GROUP NAMES                          
         OI    SFSNM1H+6,X'80'                                                  
         OI    SFSNM1H+4,X'20'                                                  
         MVC   SFSNM2,GRPGNAM2                                                  
         OI    SFSNM2H+6,X'80'                                                  
         OI    SFSNM2H+4,X'20'                                                  
         CLI   RECNUM,34            WE DOING CGROUP RECORDS?                    
         BNE   DR15                 - NOPE, WE'RE NOT                           
         XC    SFCADTE,SFCADTE     WIPE IT SQUEAKY CLEAN                        
         OC    GRPGRDAT,GRPGRDAT                                                
         BZ    DR12                                                             
         GOTO1 DATCON,DMCB,(2,GRPGRDAT),(5,SFCADTE)   CHANGED DATE              
DR12     OI    SFCADTEH+6,X'80'                                                 
         XC    SFCCWHO,SFCCWHO                                                  
         OC    GRPGRPID,GRPGRPID   ANY CHANGED PERSON?                          
         BZ    DR14                                                             
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING AUTHD,R3                                                         
         MVC   SECRAGY,SECALPHA    SECURITY AGENCY                              
         MVC   PASSWD,GRPGRPID     PERSON WHO CHANGED                           
         MVC   AIO,AIO2                                                         
         GOTO1 VALIAUTH,DMCB,WORK  GET PERSONAL ID                              
         MVC   AIO,AIO1                                                         
         MVC   SFCCWHO,PRSNLID                                                  
DR14     OI    SFCCWHOH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DR15     LA    R2,SFCLISTH                                                      
         CLI   RECNUM,34           CLIENT GROUP?                                
         BE    *+8                                                              
         LA    R2,SFSLISTH         NO, STATION GROUP                            
         TWAXC (R2),PROT=Y,TRNS=T  CLEAR LIST FIELDS                            
*                                                                               
         CLI   ACTNUM,ACTSEL       SELECT?                                      
         BNE   MK                                                               
         LA    R1,SFCPFKH                                                       
         CLI   RECNUM,34           CLIENT GROUP?                                
         BE    *+8                                                              
         LA    R1,SFSPFKH          NO, STATION GROUP                            
         OI    6(R1),X'80'                                                      
         MVC   8(21,R1),=C'PF12 = NEXT SELECTION'                               
*                                                                               
MK       XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,GRPVALCQ    LOOK FOR VALUE ELEMENTS                      
         MVC   MINEKEY+1(L'GRPKVAL),SFSSTRT                                     
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
*                                                                               
DR20     CLI   MINERR,0            ANY?                                         
         BE    DR25                YES                                          
         CLI   MINERR,MINEEOF      NONE FOUND?                                  
         BE    *+6                 RIGHT                                        
         DC    H'0'                                                             
         XC    SFSSTRT,SFSSTRT                                                  
         B     DRTS                                                             
*                                                                               
         USING GRPVALD,R6                                                       
DR25     OI    6(R2),X'80'                                                      
         CLI   RECNUM,34           CLIENT GROUP?                                
         BNE   *+14                                                             
         MVC   8(3,R2),GRPVALUE    YES - JUST PUT CLIENT CODE ON SCREEN         
         B     DR30                                                             
         MVC   8(4,R2),GRPVALUE    STATION CALL LETTERS (WITHOUT MEDIA)         
         LA    RF,12(R2)                                                        
         CLI   11(R2),C' '         HANDLE 3-CHARACTER CALL LETTERS              
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         MVI   0(RF),C'-'                                                       
         MVC   1(1,RF),GRPVALUE+4  MEDIA (OR BAND)                              
*                                                                               
DR30     ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO NEXT TWA FIELD                       
         LA    R0,SFCPFKH          A(PFKEY INSTRUCTIONS)                        
         CLI   RECNUM,34           CLIENTS?                                     
         BE    *+8                                                              
         LA    R0,SFSPFKH          A(PFKEY INSTRUCTIONS)                        
         CR    R2,R0               ANY MORE FIELDS AVAILABLE?                   
         BNL   DRS2                NO                                           
*                                                                               
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         B     DR20                                                             
*                                                                               
DRS2     MVC   SFSSTRT,GRPVALUE                                                 
         CLI   THISLSEL,C'C'       IF IT'S CHANGE FROM LIST                     
         BE    DRTS                GIVE REGULAR MESSAGE                         
         CLI   ACTEQU,ACTCHA       OR CHANGE                                    
         BE    DRTS                                                             
         CLI   ACTEQU,ACTADD       OR ADD                                       
         BE    DRTS                USE NORMAL MESSAGE                           
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMSGNO1,127        'ITEMS DISPLAYED ENTER FOR MORE'             
         MVI   GTMSYS,GTGENSYS                                                  
         DROP  RF                                                               
         OI    GENSTAT2,USMYOK+USGETTXT                                         
DRTS     OI    SFSSTRTH+6,X'80'                                                 
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* DISPLAY KEY   * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
DK       L     R4,AIO                                                           
         USING GRPKEY,R4                                                        
         OC    GRPKCODE(2),GRPKCODE                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   SVKEY2,GRPKEY                                                    
*                                                                               
         OI    SFSMEDH+6,X'80'                                                  
         OI    SFSIDH+6,X'80'                                                   
         OI    SFSCODEH+6,X'80'                                                 
         OI    SFSSTRTH+6,X'80'                                                 
*                                                                               
         MVC   GRPCODE,GRPKCODE                                                 
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
         MVC   SFSCODE(0),CODECHAR+1  CODE TO SCREEN                            
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(4),0(R4)                                                     
         GOTO1 HIGH                                                             
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
         L     RF,ABK1FH           DISPLAY BREAK NAMES AND LENGTHS              
         MVC   8(L'SFSBK1,RF),GRPBK1                                            
         L     RF,ABK1LFH                                                       
         MVI   8(RF),C'('                                                       
         MVC   9(1,RF),GRPBK1LN                                                 
         OI    9(RF),X'F0'                                                      
         MVI   10(RF),C')'                                                      
         CLI   GRPBK2LN,0          BREAK 2 MAY NOT BE THERE                     
         BE    DKX                                                              
*                                                                               
         L     RF,ABK2FH                                                        
         MVC   8(L'SFSBK2,RF),GRPBK2                                            
         L     RF,ABK2LFH                                                       
         MVI   8(RF),C'('                                                       
         MVC   9(1,RF),GRPBK2LN                                                 
         OI    9(RF),X'F0'                                                      
         MVI   10(RF),C')'                                                      
         DROP  R6                                                               
*                                                                               
DKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* LIST RECORDS   * * * * * * * * * * * * * * * * * * * * * * * * * * *          
*                                                                               
LR       LA    R4,KEY                                                           
         USING GRPKEY,R4           BUILD GROUP RECORD KEY                       
         XC    GRPKELCD(GRPKELQL),GRPKELCD  FIRST MINIO RECORD                  
         CLI   GRPKTYP,GRPKTYPQ    STILL LOOKING AT GROUP RECORDS?              
         BNE   LR10                                                             
         LA    R0,GRPKCTYQ                                                      
         CLI   RECNUM,34                                                        
         BE    *+8                                                              
         LA    R0,GRPKSTYQ                                                      
         CLM   R0,1,GRPKSTYP                                                    
         BNE   LR10                NO                                           
         OC    GRPKCODE,GRPKCODE                                                
         BNZ   KSV2                                                             
*                                                                               
LR10     MVI   GRPKTYP,GRPKTYPQ    BUILD KEY                                    
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,GRPID        ID/CODE                                      
         MVC   GRPKCODE,STRTCODE   GROUP                                        
         MVI   GRPKSTYP,GRPKCTYQ                                                
         CLI   RECNUM,34           CLIENT GROUP?                                
         BE    KSV2                                                             
         MVI   GRPKSTYP,GRPKSTYQ   STATION GROUP                                
*                                                                               
KSV2     MVC   SAVEKEY,GRPKEY                                                   
         GOTO1 HIGH                                                             
         B     LRS                                                              
*                                                                               
LRGR     CLC   GRPKEY(GRPKMSQL),SAVEKEY    DID WE PROCESS ONE FROM              
         BE    LSQ                         THIS MINIO SET ALREADY?              
LRS      CLC   GRPKEY(4),SAVEKEY                                                
         BNE   LRX                                                              
         MVC   SAVEKEY,GRPKEY                                                   
*                                                                               
         GOTO1 GETREC                                                           
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
         MVC   LSTCODE(0),CODECHAR+1  CODE TO SCREEN LINE BLANK PADDED          
         L     R6,AIO                                                           
         USING GRPGRPD,R6                                                       
         MVI   ELCODE,GRPGRPCQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LSTNAME1(20),GRPGNAM1  GROUP NAMES TO SCREEN                     
         OC    GRPGNAM2,GRPGNAM2                                                
         BZ    *+10                                                             
         MVC   LSTNAME2(20),GRPGNAM2                                            
*                                                                               
         GOTO1 LISTMON                                                          
*                                                                               
LSQ      GOTO1 SEQ                 NEXT GROUP RECORD                            
         B     LRGR                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
* PRINT RECORDS * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
PR       MVI   DOWNLOAD,C'N'                                                    
         ICM   RE,15,TWAMASTC                                                   
         USING MASTD,RE                                                         
         L     RF,MCVREMOT         GET REMOTED ADDRESS                          
         USING REMOTED,RF                                                       
         TM    REMOTTYP,X'18'      TEST OUTPUT TYPE = DOWN OR SQL               
         BZ    *+8                                                              
         MVI   DOWNLOAD,C'Y'                                                    
         DROP  RE,RF                                                            
*                                                                               
         XC    HEADHOOK,HEADHOOK                                                
         XC    SPECS,SPECS                                                      
         CLI   DOWNLOAD,C'Y'                                                    
         BE    PR1                                                              
                                                                                
PR0      LA    R3,HEADING          SET                                          
         ST    R3,SPECS                UP                                       
         LA    R3,HDHK                    REPORT                                
         ST    R3,HEADHOOK                    HEADINGS                          
*                                                                               
PR1      XC    LASTGRP,LASTGRP                                                  
         CLI   GRPID,0             TEST ALL GROUP REQUEST                       
         BNE   PR2                 NO                                           
         LA    RE,SPCGRTAB-3                                                    
         ST    RE,LASTGRP          SET TO GO IN ALPHA SEQ                       
*                                                                               
PR2      XC    SVKEY2,SVKEY2                                                    
         LA    R4,SVKEY2           CLEAR KEY                                    
         USING GRPKEY,R4                                                        
         MVI   GRPKTYP,GRPKTYPQ    RECORD TYPE                                  
*                                                                               
         MVI   GRPKSTYP,GRPKCTYQ                                                
         CLI   RECNUM,34           CLIENT GROUP?                                
         BE    *+8                                                              
         MVI   GRPKSTYP,GRPKSTYQ                                                
*                                                                               
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,GRPID                                                     
         OC    LASTGRP,LASTGRP     TEST ONE GROUP                               
         BZ    PR6                 YES                                          
*                                                                               
PR4      L     RE,LASTGRP          TRY NEXT ALPHA GROUP                         
         LA    RE,3(RE)                                                         
         ST    RE,LASTGRP                                                       
         LA    R0,SPCGRTBX                                                      
         CR    RE,R0                                                            
         BNL   PRX                                                              
         MVC   GRPKID,2(RE)                                                     
         MVC   GRPID,2(RE)         SAVE CURRENT GROUP ID                        
*                                                                               
         MVC   KEY(13),SVKEY2                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      TYP/SUBTYP/A-M/GRPID                         
         BE    PR8                                                              
         B     PR4                                                              
*                                                                               
PR6      MVC   KEY(13),SVKEY2                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(3),SVKEY2                                                    
         BE    PR8                                                              
         OC    LASTGRP,LASTGRP     TEST ALL GROUP REQ                           
         BNZ   PR2                 TRY NEXT                                     
         B     PRX                 ELSE DONE                                    
*                                                                               
PR8      MVC   AIO,AIO2            READ GROUP DEFINITION RECORD                 
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
         BE    PR10                                                             
         MVC   SH7+31(12),GRPBK2                                                
         MVI   SH7+44,C'('                                                      
         MVC   SH7+45(1),GRPBK2LN                                               
         OI    SH7+45,X'F0'                                                     
         MVI   SH7+46,C')'                                                      
         DROP  R6                                                               
*                                                                               
PR10     MVC   SH7+56(7),=C'CLIENTS'                                            
         CLI   RECNUM,34              CLIENTS?                                  
         BE    *+10                                                             
         MVC   SH7+56(8),=C'STATIONS'                                           
         OC    SH7,SPACES                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GRPKEY,R4                    GROUP RECORD KEY                    
         MVI   GRPKTYP,GRPKTYPQ             BUILD KEY                           
         XC    GRPKELCD(GRPKELQL),GRPKELCD  FIRST MINIO RECORD                  
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,GRPID                                                     
         MVC   GRPKCODE,=X'0001'   FIRST GROUP RECORD                           
         MVI   GRPKSTYP,GRPKCTYQ                                                
*                                                                               
         CLI   RECNUM,34           CLIENT GROUP?                                
         BE    *+8                                                              
         MVI   GRPKSTYP,GRPKSTYQ   STATION GROUP                                
         MVC   SAVEKEY,GRPKEY                                                   
         MVI   GOTU,C'N'                                                        
*                                                                               
         GOTO1 HIGH                                                             
         DROP  R4                                                               
*                                                                               
PRGR     LA    R4,KEY                                                           
         USING GRPKEY,R4                                                        
*                                                                               
         CLC   GRPKEY(4),SAVEKEY   PART OF SAME GROUP ID                        
         BE    PR14                                                             
         CLI   DOWNLOAD,C'Y'                                                    
         BE    PR12                                                             
         CLI   GOTU,C'Y'           GOT ONE TO UNDERLINE?                        
         BNE   PR12                                                             
         OC    ABOX,ABOX                                                        
         BZ    PR12                                                             
         L     R3,ABOX             A(BOX DSECT)                                 
         USING BOXD,R3                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'          BOTTOM                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R3                                                               
*                                                                               
PR12     OC    LASTGRP,LASTGRP     TEST ONE GROUP REQ                           
         BZ    PRX                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     PR2                                                              
*                                                                               
PR14     MVC   SAVEKEY,GRPKEY                                                   
*                                                                               
         CLI   DOWNLOAD,C'Y'                                                    
         BE    PR18                                                             
         CLI   GOTU,C'Y'                                                        
         BNE   PR17                                                             
         CLI   LINE,55                                                          
         BL    PRM                                                              
         OC    ABOX,ABOX                                                        
         BZ    PR17                                                             
         L     R3,ABOX             A(BOX DSECT)                                 
         USING BOXD,R3                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)    PRINT HORIZONTAL LINE                        
         MVI   0(R1),C'B'          BOTTOM                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R3                                                               
         MVI   FORCEHED,C'Y'                                                    
         B     PR17                                                             
*                                                                               
PRM      OC    ABOX,ABOX                                                        
         BZ    PR17                                                             
         L     R3,ABOX             A(BOX DSECT)                                 
         USING BOXD,R3                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)    PRINT HORIZONTAL LINE                        
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R3                                                               
*                                                                               
PR17     MVI   GOTU,C'Y'                                                        
*                                                                               
PR18     GOTO1 GETREC                                                           
*                                                                               
         ICM   R1,B'1100',GRPKCODE                                              
         SRL   R1,12                  DD DD ?? ??  =>  00 0D DD D?              
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'           00 0D DD DS                               
         UNPK  CODECHAR(5),FULL+1(3)               =>  Z0 ZD ZD ZD ZD           
*                                                                               
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
         LA    R6,ELEM                                                          
         MVC   MINMKEY(GRPKMSQL),SAVEKEY                                        
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,GRPVALCQ    LOOK FOR VALUE ELEMENTS                      
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
*                                                                               
PR20     CLI   MINERR,0            ANY?                                         
         BE    PR25                YES                                          
         CLI   MINERR,MINEEOF      NONE FOUND?                                  
         BE    PRS2                RIGHT                                        
         DC    H'0'                                                             
*                                                                               
PR25     MVI   DFLG,C'Y'                                                        
         USING GRPVALD,R6                                                       
         CLI   RECNUM,34           CLIENT GROUP?                                
         BNE   PR27                NO                                           
         MVC   0(3,R2),GRPVALUE    YES -  PUT CLIENT CODE ON SCREEN             
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   PR30                                                             
* FOR DOWNLOAD, GET CLIENT NAME                                                 
         MVC   SVDIRKEY,KEY        SAVE CURRENT KEY                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         GOTO1 CLPACK,DMCB,GRPVALUE,KEY+2                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PR25A                                                            
         L     RE,AIO2                                                          
         MVC   CNAME-CLTHDRD(20,RE),=CL20'** UNKNOWN **'                        
         B     PR25X                                                            
*                                                                               
PR25A    MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
                                                                                
* FOR DOWNLOAD BUILD TRANSMIT DATA IN SVCLIST                                   
                                                                                
PR25X    MVC   KEY,SVDIRKEY         RESTORE PREVIOUS KEY                        
         GOTO1 HIGH                                                             
*                                                                               
         XC    SVCLIST,SVCLIST                                                  
         LA    R3,SVCLIST                                                       
         USING CGRPDATA,R3                                                      
         MVC   CGRPMED,QMED                                                     
         CLC   P1(4),SPACES         TEST DATA IN PRINTLINE                      
         BNH   PR26                 NO - PRESERVE OLD                           
         MVC   CGRPCOD1,SPACES                                                  
         MVC   CGRPNAM1,P1+6                                                    
         BAS   RE,PRGETID                                                       
         MVC   CGRPCOD1(2),0(R1)       GROUP ID                                 
         LA    R1,CGRPCOD1+1                                                    
         CLI   0(R1),C' '              TEST 2 CHAR GROUP                        
         BNH   *+8                                                              
         AHI   R1,1                                                             
         MVC   0(4,R1),P1+1            AND NUMBER                               
*                                                                               
         MVC   CGRPCOD2,SPACES                                                  
         MVC   CGRPNAM2,P1+31                                                   
         CLC   CGRPNAM2,SPACES                                                  
         BNH   PR26                    SUPPRESS GROUP CODE IF NO NAME           
         BAS   RE,PRGETID                                                       
         MVC   CGRPCOD2(2),0(R1)       GROUP ID                                 
         LA    R1,CGRPCOD2+1                                                    
         CLI   0(R1),C' '              TEST 2 CHAR GROUP                        
         BNH   *+8                                                              
         AHI   R1,1                                                             
         MVC   0(4,R1),P1+1            AND NUMBER                               
*                                                                               
PR26     MVC   CGRPCLT,GRPVALUE                                                 
         L     RE,AIO2                                                          
         MVC   CGRPCNAM,CNAME-CLTHDRD(RE)                                       
         DROP  R3                                                               
*                                                                               
         LA    R4,CGRECTAB         POINT TO TABLE                               
         BRAS  RE,OUTPUT                                                        
         B     NXM                                                              
*                                                                               
PR27     MVC   0(4,R2),GRPVALUE    STATION CALL LETTERS (WITHOUT MEDIA)         
         LA    RF,4(R2)                                                         
         CLI   3(R2),C' '          HANDLE 3-CHARACTER CALL LETTERS              
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         MVI   0(RF),C'-'                                                       
         MVC   1(1,RF),GRPVALUE+4  MEDIA (OR BAND)                              
         CLI   GRPVALUE+4,C'T'                                                  
         BNE   *+12                                                             
         MVI   2(RF),C'V'                                                       
         B     PR30                                                             
         MVI   2(RF),C'M'                                                       
         DROP  R6                                                               
*                                                                               
PR30     CLI   RECNUM,34           CLIENTS?                                     
         BE    PBC                                                              
         LA    R0,P1+123           WILL ONE MORE FIT ON THIS LINE?              
         LA    R2,8(R2)                                                         
         B     PC                                                               
PBC      LA    R0,P1+127                                                        
         LA    R2,5(R2)                                                         
PC       CR    R2,R0               ANY MORE FIELDS AVAILABLE?                   
         BL    NXM                 YES                                          
         MVI   CONT,C'Y'                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   DFLG,C'N'                                                        
         LA    R2,P1+56                                                         
*                                                                               
NXM      GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         B     PR20                YES                                          
*                                                                               
PRS2     CLI   DFLG,C'Y'                                                        
         BNE   PSQ                                                              
         CLI   DOWNLOAD,C'Y'                                                    
         BE    PSQ                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   DFLG,C'N'                                                        
*                                                                               
PSQ      XC    KEY,KEY                                                          
         MVC   KEY(GRPKMSQL),SAVEKEY                                            
         SR    R1,R1                                                            
         ICM   R1,B'0011',KEY+4                                                 
         LA    R1,1(R1)                                                         
         STCM  R1,B'0011',KEY+4                                                 
         GOTO1 HIGH                                                             
         B     PRGR                                                             
*                                                                               
PRX      CLI   DOWNLOAD,C'Y'       TEST DOWNLOADING                             
         BNE   XIT                                                              
         SR    R4,R4               CLEAR TABLE ADDRESS                          
         BRAS  RE,OUTPUT                                                        
         B     XIT                                                              
         DROP  R4                                                               
                                                                                
*                                                                               
PRGETID  LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
PRGETID2 CLC   SAVEKEY+3(1),2(R1)                                               
         BER   RE                                                               
         LA    R1,3(R1)                                                         
         BCT   R0,PRGETID2                                                      
         DC    H'0'                                                             
         EJECT                                                                  
* HEADER ROUTINE    * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
HDHK     NTR1                                                                   
         MVC   H1+10(10),MEDNM                                                  
         OC    H1+10(10),SPACES                                                 
         MVC   H2+60(2),=C'ID'                                                  
         BAS   RE,TRANS12                                                       
         MVC   H2+63(2),0(R1)                                                   
         CLI   RECNUM,34           CLIENT GROUP?                                
         BNE   *+20                                                             
         MVC   H1+52(20),=C'CLIENT GROUP RECORDS'                               
         MVC   H3+52(20),=C'--------------------'                               
         B     *+16                                                             
         MVC   H1+52(21),=C'STATION GROUP RECORDS'                              
         MVC   H3+52(21),=C'---------------------'                              
         MVI   H5,0                                                             
         MVC   H7,SH7                                                           
         CLI   CONT,C'Y'                                                        
         BNE   BX                                                               
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
HDHKX    XIT1                                                                   
*                                                                               
TRANS12  LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
TRANS12A CLC   GRPID,2(R1)                                                      
         BER   RE                                                               
         LA    R1,3(R1)                                                         
         BCT   R0,TRANS12A                                                      
         DC    H'0'                                                             
*                                                                               
HEADING  SSPEC H1,1,C'MEDIA'                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H3,95,RUN                                                        
         SSPEC H4,95,REPORT                                                     
         SSPEC H4,112,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
CTDISP   DC    Y(L'SAPEKEY+L'SAPELEN+L'SAPESTAT)                                
         GETEL2 R6,CTDISP,ELCODE                                                
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
MYERR    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONHEAD+10(0),1(R1)                                              
         GOTO1 ERREX2                                                           
*                                                                               
BADNUMDG BAS   R1,MYERR                                                         
         DC    AL1(BADNUMDX-BADNUMDM)                                           
BADNUMDM DC    C'NUMBER OF DIGITS MUST EQUAL SUM OF BREAK LENGTHS'              
BADNUMDX EQU   *                                                                
*                                                                               
BADGRPCD BAS   R1,MYERR                                                         
         DC    AL1(BADGRPCX-BADGRPCM)                                           
BADGRPCM DC    C'GROUP CODE MUST BE NON-ZERO'                                   
BADGRPCX EQU   *                                                                
*                                                                               
NOBREAK2 BAS   R1,MYERR                                                         
         DC    AL1(NOBREAKX-NOBREAKM)                                           
NOBREAKM DC    C'BREAK 2 IS NOT DEFINED'                                        
NOBREAKX EQU   *                                                                
*                                                                               
NODELETE BAS   R1,MYERR                                                         
         DC    AL1(NODELETX-NODELETM)                                           
NODELETM DC    C'NOT A MEMBER OF THIS GROUP'                                    
NODELETX EQU   *                                                                
*                                                                               
NOADD    BAS   R1,MYERR                                                         
         DC    AL1(NOADDX-NOADDM)                                               
NOADDM   DC    C'ALREADY A MEMBER OF THIS GROUP'                                
NOADDX   EQU   *                                                                
*                                                                               
NODUP    BAS   R1,MYERR                                                         
         DC    AL1(NODUPX-NODUPM)                                               
NODUPM   DC    C'CANNOT HAVE DUPLICATES'                                        
NODUPX   EQU   *                                                                
*                                                                               
BADCODE  BAS   R1,MYERR                                                         
         DC    AL1(BADCODEX-BADCODEM)                                           
BADCODEM DC    C'NOT A VALID GROUP CODE - CHECK DOCUMENTATION'                  
BADCODEX EQU   *                                                                
*                                                                               
BADUSER  BAS   R1,MYERR                                                         
         DC    AL1(BADUSERX-BADUSERM)                                           
BADUSERM DC    C'YOU ARE NOT AUTHORIZED TO MAKE ANY CHANGES'                    
BADUSERX EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*        USERCHK SUBROUTINE                                           *         
***********************************************************************         
USERCHK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    ASECBLK,ASECBLK                                                  
         BZ    UCHKNO              NO SECURITY NO CHANGE                        
**       DC    H'0'                                                             
         L     R1,ASECBLK                                                       
         USING SECD,R1                                                          
         MVC   PIDNAME,SECPID                                                   
         DROP  R1                                                               
*                                                                               
         BRAS  RE,CHECKPID         NEED THE 2 BYTE PID NOW                      
         BNE   UCHKNO                                                           
*                                                                               
         XC    KEY2,KEY2                                                        
         MVC   KEY2(L'KEY),KEY                                                  
         XC    KEY,KEY                                                          
         CLC   SVKEY2(2),=X'0D04'   RECNUM IS 34, MUST BE CLIENT GROUP          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(4),SVKEY2       CLIENT GROUP DEFINITION RECORD KEY           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      SHOULD BE THERE                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         USING GRPKEY,R6                                                        
         L     R6,AIO2             WE NEED THE GROUP DEFINITION RECORD          
         MVI   ELCODE,GRPSCMCQ     X'12' SECURITY MANAGER ID ELEMENT            
         BRAS  RE,GETEL                                                         
         BNE   UCHKYES             NO RESTRICTION, WE'RE GOOD TO GO             
         USING GRPSCMD,R6                                                       
         OC    GRPSCM(12),GRPSCM   ANYTHING HERE?                               
         BZ    UCHKYES              - NOPE, WE OK                               
*                                                                               
         LA    RF,6                BCT LOOP 6 TIMES                             
         LA    R2,GRPSCM1          START WITH 1ST MANAGER                       
*                                                                               
UCHK50   CLC   PIDNUM,0(R2)        SAME PERSON?                                 
         BE    UCHKYES              - YUP, WE GOOD                              
         LA    R2,L'GRPSCM(R2)     BUMP NOW                                     
         BCT   RF,UCHK50                                                        
*                                                                               
UCHKNO   LTR   RE,RE                                                            
         B     *+6                                                              
*                                                                               
UCHKYES  CR    RE,RE                                                            
         J     EXIT                                                             
***********************************************************************         
*        CHECKPID SUBROUTINE                                          *         
***********************************************************************         
CHECKPID NTR1  BASE=*,LABEL=*                                                   
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         USING SAPEREC,R6                                                       
         XC    SAPEKEY,SAPEKEY     BUILD PERSON KEY                             
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SECALPHA                                                 
         MVC   SAPEPID,PIDNAME                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),=C'CTFILE ',KEY2,AIO2               
         L     R6,AIO2                                                          
         CLC   KEY2(SAPEDEF-SAPEKEY),0(R6)                                      
         BNE   CHKPIDNO                                                         
*                                                                               
         USING SAPWDD,R6                                                        
         MVI   ELCODE,SAPWDELQ     X'C4' - PERSON PASSWORD ELEM                 
         BRAS  RE,GETEL2                                                        
         BNE   CHKPIDNO                                                         
         MVC   PIDNUM,SAPWDNUM                                                  
*                                                                               
CHKPIDYS CR    RE,RE                                                            
         B     *+6                                                              
*                                                                               
CHKPIDNO LTR   RE,RE                                                            
EXIT     XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*=================================================================              
*                                                                               
* INTERFACE TO DLFLD FOR DOWNLOADS                                              
* THIS CODE IS OFFLINE ONLY -- SOON AND OV !!!!                                 
*                                                                               
* R4 POINTS TO DATA TABLE  (ZERO TO CLOSE)                                      
* R3 POINTS TO DATA AREA - MODIFY ADDRESSES BY THIS VALUE                       
*                                                                               
*=================================================================              
                                                                                
OUTPUT   NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
*                                                                               
D        USING DLCBD,DLCB                                                       
*                                                                               
         LTR   R4,R4               TEST CLOSE                                   
         BNZ   OUTPUT0             NO                                           
         CLI   DOWNINIT,C'Y'       TEST INITIALIZED                             
         BNE   OUTPUTX                                                          
         MVI   D.DLCBACT,C'R'      SET E-O-R                                    
         GOTO1 =V(DLFLD),DLCB                                                   
         MVI   DOWNINIT,C'N'       RESET INITIALIZED FLAG                       
         B     OUTPUTX                                                          
*                                                                               
OUTPUT0  CLI   DOWNINIT,C'Y'                                                    
         BE    OUTPUT2                                                          
         MVI   DOWNINIT,C'Y'                                                    
*                                                                               
         XC    DLCB,DLCB                                                        
         MVI   D.DLCBACT,C'I'      START AND INITIALIZE REPORT                  
         LA    RE,DOWNPRT                                                       
         ST    RE,D.DLCBAPR        PRINT ROUTINE ADDRESS                        
         LA    R0,P1                                                            
         ST    R0,D.DLCBAPL        PRINT LINE ADDRESS                           
         OI    D.DLCBFLG1,DLCBFXTN                                              
         MVC   D.DLCXTND(7),MAXLINE                                             
                                                                                
         GOTO1 VDLFLD,DLCB                                                      
                                                                                
         XC    DMCB(8),DMCB        GET EDITOR ADDRES                            
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QEDITOR                                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   D.DLCBAED,4(R1)     DLFLD REQUIRES A(EDITOR)                     
*                                                                               
OUTPUT2  CLI   DOWNFRST,0                                                       
         BNE   OUTPUT10                                                         
*                                                                               
         MVI   DOWNFRST,C'N'                                                    
         MVI   FORCEHED,C'Y'       SKIP TO NEW PAGE AND PRINT NOTHING!          
         LA    R0,14                                                            
         LA    R1,HEAD1                                                         
         MVC   0(132,R1),SPACES                                                 
         AHI   R1,132                                                           
         BCT   R0,*-10                                                          
*                                                                               
OUTPUT10 L     RE,0(R4)            GET DATA ADDR                                
         AR    RE,R3               POINT TO DATA                                
         SR    RF,RF                                                            
         IC    RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   D.DLCBFLD(0),0(RE)                                               
*                                                                               
         CLI   5(R4),C'T'          TEST TEXT                                    
         BE    *+6                 ONLY SUPPORT TEXT FOR NOW                    
         DC    H'0'                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    D.DLCBFLD(0),SPACES                                              
*                                                                               
OUTPUT30 MVC   D.DLCBTYP(1),5(R4)    SET DATA TYPE                              
         MVI   D.DLCBACT,DLCBPUT                                                
*                                                                               
         GOTO1 VDLFLD,DLCB                                                      
         MVI   D.DLCXDELC,C' '       ALWAYS RESTORE TERMINATOR                  
*                                                                               
         AHI   R4,8                                                             
         CLI   0(R4),X'FF'                                                      
         BNE   OUTPUT2                                                          
*                                                                               
OUTPUT32 MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
OUTPUTX  XIT1                                                                   
         LTORG                                                                  
*                                                                               
DOWNPRT  NTR1  BASE=*,LABEL=*                                                   
         LM    R8,RA,DOWNPRR8                                                   
         MVI   LINE,0              FORCE NO PAGE BREAK                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'N'       AND NO HEADLINES !                           
         MVI   FORCEMID,C'N'       OR MIDLINES !                                
         XIT1                                                                   
DOWNPRR8 DS    A                                                                
DOWNPRR9 DS    A                                                                
DOWNPRRA DS    A                                                                
         LTORG                                                                  
                                                                                
MAXLINE  DC    H'132'              MAX LINE WIDTH                               
DELIM    DC    C' '                FIELD DELIMITER CHR                          
EOTCHR   DC    C'"'                END OF TEXT FIELD DELIMITER                  
EOTALT   DC    C''''               END OF TEXT CHR ALTERNATE                    
EOLCHR   DC    X'5E'               END OF LINE CHAR - SEMICOLON                 
EORCHR   DC    C':'                END OF REPORT CONTROL CHR                    
*=================================================================              
* TABLE OF DATA DISPLACEMENTS AND LENGTHS FOR CLTGRP DOWNLOAD                   
*=================================================================              
                                                                                
CGRECTAB DS    0D                                                               
         DC    A(CGRPMED-CGRPDATA),AL1(L'CGRPMED),C'T',XL2'00'                  
         DC    A(CGRPCOD1-CGRPDATA),AL1(L'CGRPCOD1),C'T',XL2'00'                
         DC    A(CGRPNAM1-CGRPDATA),AL1(L'CGRPNAM1),C'T',XL2'00'                
         DC    A(CGRPCOD2-CGRPDATA),AL1(L'CGRPCOD2),C'T',XL2'00'                
         DC    A(CGRPNAM2-CGRPDATA),AL1(L'CGRPNAM2),C'T',XL2'00'                
         DC    A(CGRPCLT-CGRPDATA),AL1(L'CGRPCLT),C'T',XL2'00'                  
         DC    A(CGRPCNAM-CGRPDATA),AL1(L'CGRPCNAM),C'T',XL2'00'                
         DC    X'FF'                                                            
*                                                                               
       ++INCLUDE SPCGRTAB                                                       
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMC6D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMC7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMB4D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
MINIO    DS    V                   A(MINIO)                                     
VDLFLD   DS    V                                                                
ABK1FH   DS    A                   A(BREAK 1 NAME FIELD HEADER)                 
ABK1LFH  DS    A                   A(BREAK 1 LENGTH FIELD HEADER)               
ABK2FH   DS    A                   A(BREAK 2 NAME FIELD HEADER)                 
ABK2LFH  DS    A                   A(BREAK 2 LENGTH FIELD HEADER)               
LASTGRP  DS    A                                                                
GRPID    DS    C                   GROUP ID                                     
GRPCODE  DS    XL2                 GROUP CODE (PWOS)                            
STRTCODE DS    XL2                 START GROUP CODE FOR LIST                    
PIDNAME  DS    CL8                 USER'S PIDNAME                               
PIDNUM   DS    XL2                 SAVE OFF USER'S PID                          
SECALPHA DS    CL2                 SECURITY AGENCY ALPHA                        
KEY2     DS    CL50                                                             
SAVEKEY  DS    XL13                                                             
SVKEY2   DS    XL20                                                             
SVDIRKEY DS    XL13                                                             
CODECHAR DS    CL5                 GROUP CODE CHARACTER                         
BREAK1LN DS    X                   BREAK 1 LENGTH FROM DEFINITION REC           
BREAK2LN DS    X                   BREAK 2 LENGTH FROM DEFINITION REC           
VALUES   DS    XL((15*VALUELNQ)+1) SAVED VALUES FROM SCREEN (MAX = 15)          
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAG 1                         
MF1UPD   EQU   X'80'                - RECORD IS UPDATED                         
MYWORK   DS    XL64                                                             
SH7      DS    CL132                                                            
SVNMS    DS    CL55                                                             
DFLG     DS    C                   STUFF TO PRINT                               
CONT     DS    C                   CONTINUATION FLAG                            
GOTU     DS    C                   GOT ONE TO UNDERLINE FLAG                    
DOWNLOAD DS    C                   C'Y' IF DOWNLOADING                          
DOWNINIT DS    C                                                                
DOWNFRST DS    C                                                                
         DS    0D                                                               
DLCB     DS    XL256                                                            
         DS    0D                                                               
MINBLOCK DS    XL(MINBLKL)         MINIO PARAMETER BLOCK                        
MINBLCKX EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
       ++INCLUDE DDMINBLK                                                       
         EJECT                                                                  
VALUED   DSECT                                                                  
*                                                                               
VALUEVAL DS    CL(L'GRPPVAL)       VALUE TO BE STORED IN RECORD/POINTER         
VALUEACT DS    C                   USER ACTION (C'+' OR C'-')                   
VALUEDEL DS    XL2                 GRP CODE FROM WHICH WE'LL DELETE IT          
*                                                                               
VALUELNQ EQU   *-VALUED                                                         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTCODE  DS    CL4                                                              
         DS    CL2                                                              
LSTNAME1 DS    CL24                                                             
         DS    CL4                                                              
LSTNAME2 DS    CL24                                                             
*                                                                               
CGRPDATA DSECT                                                                  
CGRPMED  DS    CL1                                                              
CGRPCOD1 DS    CL5                                                              
CGRPNAM1 DS    CL24                                                             
CGRPCOD2 DS    CL5                                                              
CGRPNAM2 DS    CL24                                                             
CGRPCLT  DS    CL3                                                              
CGRPCNAM DS    CL20                                                             
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPSFM26   03/29/06'                                      
         END                                                                    

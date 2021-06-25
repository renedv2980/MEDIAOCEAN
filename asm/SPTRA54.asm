*          DATA SET SPTRA54    AT LEVEL 025 AS OF 05/01/02                      
*PHASE T21654A                                                                  
         TITLE 'T21654 - SPOT RECAP'                                            
*                                                                               
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - READ COMMERCIAL RECORDS                                    
*                                                                               
*             AIO3 -                                                            
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG                                                          
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE REG                                                   
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
*                                                                               
*  LEV 17    NOV01/88 BYPASS AFFIDAVIT ELEMS TO LOOK FOR DLR TAG ELEMS          
*  LEV 18    DEC22/92 FIX FOR MSUNPK                                  *         
*  LEV 19    JAN04/93 BYPASS DEALER TAG ELEMENTS AND PIGGYBACKS       *         
*  LEV 20    JAN07/93 FIX BUG OVERRUNNING TABLE SIZE                  *         
*  LEV 21    JUN07/93 ALLOW FOR UNTAGGED/NO COMML SPOTS               *         
*  LEV 22    FEB18/94 ADD STRAFFIC                                    *         
*  LEV 23    JUL21/94 CHANGE TO FILENAME                              *         
*  LEV 24 BGRI MAY07/01 CHANGE DUMMY                                  *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21654   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21654**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR54RR                                                      
         LA    R1,ASVNEXT                                                       
         ST    R1,ASVSTOR                                                       
         LH    R0,=AL2(L'SPTABLE)                                               
         LA    R1,SPTABLE                                                       
         CLI   OFFLINE,C'Y'                                                     
         BNE   INIT10                                                           
         L     R1,VADUMMY                                                       
         LH    R0,=H'26000'                                                     
         SPACE                                                                  
INIT10   ST    R1,ASPTABLE                                                      
         AR    R1,R0                                                            
         ST    R1,ASPTABND                                                      
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFFLINE INSTRUCTIONS                         
         BE    LRR                                                              
         CLI   MODE,LISTRECS                                                    
         BE    LRL                                                              
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY FIELDS                                                           
         SPACE                                                                  
VK       TM    TRAMEDH+4,X'20'                                                  
         BZ    VK04                                                             
         TM    TRACLTH+4,X'20'                                                  
         BZ    VK04                                                             
         TM    TRAPRDH+4,X'20'                                                  
         BZ    VK04                                                             
         TM    TRAMKTH+4,X'20'                                                  
         BZ    VK04                                                             
         TM    TRAPERH+4,X'20'                                                  
         BO    EXIT                                                             
VK04     LA    R2,TRAMEDH          MEDIA                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VK10     LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VK20     LA    R2,TRAPRDH          PRODUCT                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK                                                     
         BE    PRDINV                                                           
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         MVC   PRDNM,WORK+5                                                     
         OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VK30     LA    R2,TRAMKTH          MARKET                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
         GOTO1 VALIMKT                                                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VK40     LA    R2,TRAPERH          PERIOD                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
         BAS   RE,VPER                                                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
         SPACE                                                                  
VK60     DS    0H                                                               
         SPACE                                                                  
* READ BUYS, LOOKING FOR ANY DEALER TAG RECORDS                                 
         SPACE                                                                  
         L     R3,ASPTABLE                                                      
         BAS   RE,RDBUYS                                                        
         BNE   NOSPTER             IF NONE FOUND, ERROR                         
         SPACE                                                                  
         CLI   OFFLINE,C'Y'                                                     
         BE    EXIT                                                             
         SPACE                                                                  
         XC    ASVNEXT,ASVNEXT                                                  
         BAS   RE,SVTWA                                                         
         SPACE                                                                  
         XC    KEY,KEY                                                          
         B     EXIT                                                             
         EJECT                                                                  
* ONLINE RECAP - 1 LINE PER SPOT *                                              
* FROM SPOT TABLE                                                               
         SPACE                                                                  
         USING SPTABLED,R3                                                      
LRL      MVI   USEIO,C'Y'          TELL USER I/O ONLY                           
         L     R3,ASPTABLE         START OF SPOT TABLE                          
         BAS   RE,RDTWA                                                         
         A     R3,ASVNEXT          ADD POINTER                                  
         CLI   SPTFTD,0            AT END                                       
         BNE   LRL10               NO                                           
         L     R3,ASPTABLE         START OF SPOT TABLE                          
         SPACE                                                                  
LRL10    OC    SPTTAG,SPTTAG       IS THERE ONE ASSIGNED                        
         BNZ   *+14                                                             
         MVC   LTAG,=C'NONE'                                                    
         B     LRL12                                                            
         SPACE                                                                  
         EDIT  (B2,SPTTAG),(4,LTAG),ALIGN=LEFT                                  
LRL12    GOTO1 DATCON,DMCB,(2,SPTFTD),(4,LDATES)                                
         MVI   LDATES+5,C'-'                                                    
         GOTO1 (RF),(R1),(2,SPTLTD),(4,LDATES+6)                                
         GOTO1 UNDAY,(R1),SPTDAY,LDAYS                                          
         GOTO1 UNTIME,(R1),SPTTIME,LTIMES                                       
         L     R4,AIO1                                                          
         USING BUYRECD,R4                                                       
         CLC   SPTDSKAD,BUYRLINK   BUY REC IN MEMORY                            
         BE    LRL14                                                            
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),SPTDSKAD                                               
         ST    R4,AIO                                                           
*                                                                               
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT SYSTEM                     
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   BUYRLINK,KEY+14                                                  
LRL14    MVC   LPNAME,BDPROGRM                                                  
         DROP  R4                                                               
         SPACE                                                                  
         MVC   LDAYPT,SPTDPT                                                    
         SPACE                                                                  
         EDIT  (B1,SPTSLN),(3,LSLN)                                             
         SPACE                                                                  
* SEARCH TABLE FOR COMMERCIAL RECORD  NEEDED FOR THIS SPOT *                    
         SPACE                                                                  
         L     R5,ASVCMLS                                                       
         A     R5,ASPTABLE          RELOCATE                                    
         USING SVCMLD,R5                                                        
LRL20    OC    0(L'SVCMLDTA,R5),0(R5) EMPTY SLOT                                
         BZ    LRL22                                                            
         CLC   SPTCMLSQ,SVCMLSEQ                                                
         BE    LRL26                                                            
         LA    R5,L'SVCMLDTA(,R5)                                               
         C     R5,ASPTABND                                                      
         BL    LRL20                                                            
         B     SIZERR                                                           
         SPACE                                                                  
LRL22    OC    SPTCMLSQ,SPTCMLSQ   WAS COMMERCIAL ASSIGNED                      
         BNZ   LRL24                YES                                         
         MVC   LCML+1(6),=C'*NONE*'                                             
         B     LRL28                                                            
         SPACE                                                                  
LRL24    XC    KEY,KEY             READ PASSIVE POINTER                         
         MVC   KEY(2),=X'0AA1'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(3),SPTCMLSQ                                                
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING CMLKEY,R6                                                        
         MVC   SVCMLCOD,CMLKCML                                                 
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
         TM    CMLSTAT,X'80'       TEST DELETED COMMERCIAL                      
         BZ    *+6                 NO                                           
         DS    H'0'                                                             
         CLC   SPTCMLSQ,CMLSEQ                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCMLSEQ,CMLSEQ                                                  
         LA    R1,L'SVCMLDTA(,R5)                                               
         XC    0(L'SVCMLDTA,R1),0(R1)                                           
         LA    R1,L'SVCMLDTA(,R1)                                               
         MVI   0(R1),X'FF'                                                      
         LR    RF,R1               MAKE                                         
         S     RF,ASPTABLE              ADDDRESS RELOCATABLE                    
         ST    RF,ATABEND                                                       
LRL26    MVC   LCML,SVCMLCOD                                                    
         SPACE                                                                  
LRL28    MVC   SVSTA,SPTSTA                                                     
         SPACE                                                                  
         BAS   RE,FSTA             GO FORMAT STATION FOR PRINTING               
         SPACE                                                                  
         MVC   LSTA,STAPRNT                                                     
         SPACE                                                                  
         LA    R3,L'SPTDATA(,R3)                                                
         LR    R1,R3                                                            
         S     R1,ASPTABLE                                                      
         ST    R1,ASVNEXT          SAVE RELOCATABLE ADDRESS                     
         SPACE                                                                  
         BAS   RE,SVTWA                                                         
         SPACE                                                                  
         GOTO1 LISTMON                                                          
         SPACE                                                                  
         L     R1,ATABEND          SEE IF TABLE DISAPPEARED                     
         A     R1,ASPTABLE         RELOCATE                                     
         CLI   0(R1),X'FF'         NOT IF STILL HERE                            
         BE    LRL30                                                            
         BAS   RE,RDTWA                                                         
LRL30    CLI   0(R1),X'FF'         NOT IF STILL HERE                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SPTFTD,0            END OF TABLE                                 
         BNE   LRL10               NO                                           
         SR    R0,R0                                                            
         ST    R0,ASVNEXT                                                       
         NI    TRAMEDH+4,X'FF'-X'20'                                            
         NI    TRACLTH+4,X'FF'-X'20'                                            
         NI    TRAPRDH+4,X'FF'-X'20'                                            
         NI    TRAMKTH+4,X'FF'-X'20'                                            
         NI    TRAPERH+4,X'FF'-X'20'                                            
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
* GENERATE OFFLINE SPOT RECAP *                                                 
         SPACE                                                                  
LRR      L     R3,ASPTABLE         START OF SPOT TABLE                          
         USING SPTABLED,R3                                                      
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE                                                                  
LRR06    MVI   FORCEHED,C'Y'                                                    
         MVC   SVTAG,SPTTAG        SAVE TAG FOR BREAK                           
         MVC   SVMKT,SPTMKT        MARKET                                       
         MVC   SVSTA,SPTSTA        STATION                                      
         SR    R2,R2               ZERO SPOT CTR                                
LRR10    GOTO1 DATCON,DMCB,(2,SPTFTD),(4,PDATES)                                
         MVI   PDATES+5,C'-'                                                    
         GOTO1 (RF),(R1),(2,SPTLTD),(4,PDATES+6)                                
         GOTO1 UNDAY,(R1),SPTDAY,PDAYS                                          
         GOTO1 UNTIME,(R1),SPTTIME,PTIMES                                       
         L     R4,AIO1                                                          
         USING BUYRECD,R4                                                       
         CLC   SPTDSKAD,BUYRLINK   BUY REC IN MEMORY                            
         BE    LRR14                                                            
         SPACE                                                                  
         MVC   KEY+14(4),SPTDSKAD                                               
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   BUYRLINK,KEY+14                                                  
LRR14    MVC   PPNAME,BDPROGRM                                                  
         DROP  R4                                                               
         SPACE                                                                  
         MVC   PDAYPT,SPTDPT                                                    
         SPACE                                                                  
         EDIT  (B1,SPTSLN),(3,PSLN)                                             
         SPACE                                                                  
* SEARCH TABLE FOR COMMERCIAL RECORD  NEEDED FOR THIS SPOT *                    
         SPACE                                                                  
         L     R5,ASVCMLS                                                       
         A     R5,ASPTABLE         RELOCATE                                     
         USING SVCMLD,R5                                                        
LRR20    OC    0(L'SVCMLDTA,R5),0(R5) EMPTY SLOT                                
         BZ    LRR22                                                            
         CLC   SPTCMLSQ,SVCMLSEQ                                                
         BE    LRR26                                                            
         LA    R5,L'SVCMLDTA(,R5)                                               
         C     R5,ASPTABND                                                      
         BL    LRR20                                                            
         B     SIZERR                                                           
*                                                                               
LRR22    OC    SPTCMLSQ,SPTCMLSQ   WAS COMML ASSIGNED                           
         BNZ   LRR24                YES                                         
         MVC   PCMLNM(13),=C'NONE ASSIGNED'                                     
         B     LRR28                                                            
         SPACE                                                                  
LRR24    XC    KEY,KEY             READ PASSIVE POINTER                         
         MVC   KEY(2),=X'0AA1'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(3),SPTCMLSQ                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING CMLKEY,R6                                                        
         MVC   SVCMLCOD,CMLKCML                                                 
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
         TM    CMLSTAT,X'80'       TEST DELETED COMMERCIAL                      
         BZ    *+6                 NO                                           
         DS    H'0'                                                             
         CLC   SPTCMLSQ,CMLSEQ                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCMLSEQ,CMLSEQ                                                  
         MVC   SVCMLNM,CMLTITLE                                                 
         LA    R1,L'SVCMLDTA(,R5)                                               
         XC    0(L'SVCMLDTA,R1),0(R1)                                           
LRR26    MVC   PCML,SVCMLCOD                                                    
         MVC   PCMLNM,SVCMLNM                                                   
         SPACE                                                                  
LRR28    MVC   SVSTA,SPTSTA                                                     
         SPACE                                                                  
         BAS   RE,FSTA             GO FORMAT STATION FOR PRINTING               
         SPACE                                                                  
         MVC   PSTA,STAPRNT                                                     
         BCTR  R2,0                COUNT SPOTS                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,L'SPTDATA(,R3)   LOOK FOR NEXT STA                            
         CLC   SVTAG,SPTTAG                                                     
         BE    LRR10                                                            
         LCR   R2,R2                                                            
         CH    R2,=H'1'                                                         
         BE    LRR40                                                            
         MVI   P,0                 BLANK LINE                                   
         MVC   PSLN+132(11),=CL11'TOTAL SPOTS'                                  
         LA    R4,PDAYPT+132                                                    
         EDIT  (R2),(5,(R4))                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
LRR40    CLI   SPTFTD,0            END                                          
         BNE   LRR06                                                            
         NI    TRAMEDH+4,X'FF'-X'20'                                            
         NI    TRACLTH+4,X'FF'-X'20'                                            
         NI    TRAPRDH+4,X'FF'-X'20'                                            
         NI    TRAMKTH+4,X'FF'-X'20'                                            
         NI    TRAPERH+4,X'FF'-X'20'                                            
         B     EXIT                                                             
         EJECT                                                                  
****************************************************                            
* READ THROUGH BUYS AND BUILD SPOT DEALER TAG LIST *                            
****************************************************                            
         SPACE                                                                  
RDBUYS   NTR1                                                                   
         SPACE                                                                  
* CLEAR 1ST ACTIVITY LIST BUILD AREA *                                          
         SPACE                                                                  
         XC    0(256,R3),0(R3)                                                  
         XC    SPOTCT,SPOTCT                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),BAGYMD A/M, CLT                                           
         MVC   KEY+4(2),BMKT                                                    
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'     SWITCH TO SPOT SYSTEM                  
         GOTO1 HIGH                                                             
         B     BLA12                                                            
*                                                                               
BLA10    MVC   FILENAME,=CL8'SPTDIR'     SWITCH TO SPOT SYSTEM                  
         GOTO1 SEQ                                                              
*                                                                               
BLA12    CLC   KEY(6),KEYSAVE      A-M/C/P/MKT                                  
         BNE   BLA14               DONE BUILD                                   
*                                                                               
         CLI   KEY+10,X'FF'                                                     
         BNE   BLA10                                                            
*                                                                               
         B     BLA18                                                            
         SPACE                                                                  
* SORT SPOTS IN DATE ORDER                                                      
         SPACE                                                                  
BLA14    MVI   SPOTNUMB,0                                                       
         LH    R2,SPOTCT           GET NUMBER OF SPOTS                          
         LTR   R2,R2               IF NO SPOTS FOUND                            
         BNZ   BLA16                                                            
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         CR    R2,RB               SET CC NE FOR NO SPOTS FOUND                 
         B     EXIT                                                             
         SPACE                                                                  
* SORT BY: MARKET/STATION, TAG, SPOT FTD, LTD, TIME, AND SPOT NUMBER            
         SPACE                                                                  
BLA16    L     R3,ASPTABLE                                                      
         GOTO1 XSORT,DMCB,(R3),(R2),L'SPTDATA,13,0                              
         SPACE                                                                  
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         CR    R2,R2               SET COND CODE                                
         B     EXIT                                                             
*                                                                               
BLA18    L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
         USING BUYRECD,R4                                                       
         TM    15(R4),X'80'        TEST DELETED                                 
         BO    BLA10               YES - IGNORE                                 
         SPACE                                                                  
* ONLY POL PROCESSING *                                                         
         CLI   BUYKEY+3,X'FF'      TEST POOL BUYREC                             
         BNE   BLA10                                                            
         SPACE                                                                  
* CALCULATE DIFFERENCE BETWEEN FIRST/LAST DAYS OF ROTATOR *                     
         SPACE                                                                  
         ZIC   R0,BDDAY                                                         
         SLL   R0,25                                                            
         LTR   R0,R0               REG GOES NEGATIVE WHEN BIT ARRIVES           
         BM    *+12                                                             
         SLL   R0,1                                                             
         B     *-10                                                             
*                                                                               
         SR    RE,RE               CLEAR COUNTER                                
         SLL   R0,1                                                             
         LTR   R0,R0               SHIFT TILL NO MORE BITS ON                   
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STH   RE,ROTDAYS          AND SAVE DAYS                                
         SPACE                                                                  
         MVI   SPOTNUMB,0          RESET SPOT NUMBER                            
         SPACE                                                                  
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
*                                                                               
BLA20    BAS   RE,BUYEL                                                         
         BNE   BLA10                                                            
         USING REGELEM,R6                                                       
         CLI   RLEN,10            TEST UNALL                                    
         BNH   BLA20                                                            
         TM    RSTATUS,X'C0'       TEST MINUS OR MINUSED                        
         BNZ   BLA20                                                            
         CLC   RPPRD,BPRD                                                       
         BNE   BLA20                                                            
         CLC   ELDATE,RDATE        SAME ELEM START DATE AS LAST                 
         BE    BLA22                                                            
         SR    R5,R5                                                            
BLA22    MVC   ELDATE,RDATE        SAVE ELEM START DATE                         
         MVC   ELDATEX,RDATE       AND PRESET ELEM END DATE                     
         LA    R5,1(,R5)                                                        
*                                                                               
         CLC   ELDATE,GENENDP      TEST AFTER FLIGHT/TELECAST                   
         BH    BLA20                                                            
*                                                                               
         CLI   0(R6),11            TEST REGEL                                   
         BNE   BLA24                                                            
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
         DROP  R6                                                               
*                                                                               
BLA24    CLC   ELDATEX,GENSTP      TEST BEFORE PERIOD START                     
         BL    BLA20                                                            
         SPACE                                                                  
         CLI   1(R6),18            TEST PIGGYBACK                               
         BNL   BLA20                YES, BYPASS                                 
         SPACE                                                                  
         XC    SPTWORK,SPTWORK                                                  
         LA    R3,SPTWORK                                                       
         USING SPTABLED,R3                                                      
         MVC   SPTFTD,ELDATE                                                    
         MVC   SPTLTD,ELDATEX                                                   
         STC   R5,SPTSPTN                                                       
         MVC   SPTDAY,BDDAY                                                     
         MVC   SPTTIME,BDTIMST                                                  
         MVC   SPTSLN,BDSEC                                                     
         MVC   SPTDPT,BDDAYPT                                                   
         MVC   SPTMKT,BUYMSTA                                                   
         MVC   SPTSTA,BUYMSTA+2                                                 
         MVC   SPTDSKAD,KEY+14                                                  
         SPACE                                                                  
         LR    RE,R6                                                            
         SPACE                                                                  
BLA28    ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),13            THIS ANOTHER SPOT                            
         BNH   BLA20               YES, BYPASS, NOT TAGGED YET                  
         SPACE                                                                  
         CLI   0(RE),X'18'         DEALER TAG OR SPOT ASSIGN ELEM               
         BNE   BLA28                NO, LOOK FURTHUR                            
         SPACE                                                                  
         CLI   1(RE),9             DEALER TAG (NOT SPOT ASSIGN)                 
         BNE   BLA28                NO, BYPASS                                  
         SPACE                                                                  
         MVC   SPTCMLSQ,2(RE)     SAVE CMML SEQ                                 
         MVC   SPTTAG,5(RE)          DEALER TAG                                 
         SPACE                                                                  
* TEST DATA IN TABLE ALREADY *                                                  
         SPACE                                                                  
BLA30    L     R3,ASPTABLE                                                      
*                                                                               
BLA32    CLI   SPTFTD,0                                                         
         BE    BLA40                                                            
         CLC   SPTDATA(L'SPTDATA),SPTWORK THIS ENTRY TO ALL OTHERS              
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R3,L'SPTDATA(,R3)                                                
         C     R3,ASPTABND                                                      
         BL    BLA32                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   SIZERR                                                           
         DC    H'0'                NEED LARGER TABLE                            
*                                                                               
BLA40    C     R3,ASPTABND         MAKE SURE NOT EXCEEDING TABLE SPACE          
         BL    BLA44                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   SIZERR                                                           
         DC    H'0'                NEED LARGER TABLE                            
         SPACE                                                                  
BLA44    LH    R1,SPOTCT           UPDATE TOTAL SPOTS COUNTER                   
         LA    R1,1(R1)                                                         
         STH   R1,SPOTCT                                                        
         SPACE                                                                  
         MVC   SPTDATA,SPTWORK                                                  
         XC    L'SPTDATA(L'SPTDATA,R3),L'SPTDATA(R3)                            
         LA    R1,L'SPTDATA*2(R3)                                               
         S     R1,ASPTABLE          MAKE RELOCATABLE                            
         ST    R1,ASVCMLS                                                       
         B     BLA20                                                            
*                                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
* FORMAT STATION FOR PRINTING *                                                 
         SPACE                                                                  
FSTA     NTR1                                                                   
         GOTO1 MSUNPK,DMCB,SVMKT,QMKT,WORK                                      
         MVC   QSTA,WORK                                                        
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),QSTA                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    EXIT                                                             
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    EXIT                                                             
         MVI   3(RE),C' '                                                       
         B     EXIT                                                             
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE VALIDATES START/END DATES FOR PERIOD             *                 
***************************************************************                 
         SPACE                                                                  
VPER     NTR1                                                                   
         LA    R5,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R5),SVQSTART                                        
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVPERST)                             
         SPACE                                                                  
         MVC   SVQEND,SVQSTART                                                  
         CLM   R4,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    EXIT                YES                                          
         SPACE                                                                  
         LA    R5,1(R4,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),SVQEND                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQEND),(3,SVPEREND)                              
         CLC   SVPERST,SVPEREND                                                 
         BH    DATERR                                                           
         SPACE                                                                  
         GOTO1 (RF),(R1),(3,SVPERST),(2,GENSTP)                                 
         GOTO1 (RF),(R1),(3,SVPEREND),(2,GENENDP)                               
         SPACE                                                                  
         GOTO1 (RF),(R1),(3,SVPERST),(0,USERQSTR)                               
         GOTO1 (RF),(R1),(3,SVPEREND),(0,USERQEND)                              
         B     EXIT                                                             
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
         SPACE                                                                  
HDHK     NTR1                                                                   
         MVC   H2+10(L'QMED),QMED                                               
         MVC   H2+15(L'MEDNM),MEDNM                                             
         MVC   H3+10(L'QCLT),QCLT                                               
         MVC   H3+15(L'CLTNM),CLTNM                                             
         MVC   H4+10(L'QPRD),QPRD                                               
         MVC   H4+15(L'PRDNM),PRDNM                                             
         SPACE                                                                  
         OC    SVTAG,SVTAG         WAS DEALER TAG ASSIGNED?                     
         BNZ   HDHK04               YES                                         
         MVC   H6+16(13),=C'UNTAGGED SPOT'                                      
         B     HDHKX                                                            
         SPACE                                                                  
HDHK04   LA    R4,KEY                                                           
         USING DLRKEY,R4                                                        
         MVC   DLRKID,=X'0A2C'                                                  
         MVC   DLRKAM(3),BAGYMD AND BCLT                                        
         MVC   DLRKMKT,SVMKT       MARKET ONLY                                  
         MVC   DLRKPROD,QPRD                                                    
         MVC   DLRKTAG,SVTAG                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    HDHK10                                                           
         MVC   KEY,KEYSAVE                                                      
         XC    DLRKMKT,DLRKMKT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
HDHK10   L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (B2,SVTAG),(4,H6+11)                                             
         USING DLRDTAEL,R6                                                      
         MVC   H6+16(L'DLRNAME1),DLRNAME1                                       
         LA    R1,H6+16+L'DLRNAME1-1                                            
HDHK12   CLI   0(R1),C' '                                                       
         BH    HDHK14                                                           
         BCTR  R1,0                                                             
         B     HDHK12                                                           
HDHK14   OC    DLRADDR1,DLRADDR1                                                
         BZ    HDHK15                                                           
         MVI   1(R1),C'/'                                                       
         MVC   2(L'DLRADDR1,R1),DLRADDR1                                        
HDHK15   OC    DLRNAME2,DLRNAME2   SECOND DEALER                                
         BZ    HDHK20                                                           
         MVC   H7+2(8),=C'DEALER 2'                                             
         MVC   H7+16(L'DLRNAME2),DLRNAME2                                       
         LA    R1,H7+16+L'DLRNAME2-1                                            
HDHK16   CLI   0(R1),C' '                                                       
         BH    HDHK18                                                           
         BCTR  R1,0                                                             
         B     HDHK16                                                           
HDHK18   MVI   1(R1),C'/'                                                       
         MVC   2(L'DLRADDR2,R1),DLRADDR2                                        
         B     EXIT                                                             
HDHK20   MVI   H6+9,C' '           DON'T PRINT 1 IF ONLY 1                      
         SPACE                                                                  
HDHKX    B     EXIT                                                             
         SPACE 3                                                                
* RTN TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                            
         SPACE                                                                  
SVTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    SET TERMINAL NUMBER                          
         MVI   DMCB+8,2            SET PAGE                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ASVSTOR                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         SPACE                                                                  
* RTN TO READ TWA2 WITH EXTENDED SPOT LIST *                                    
         SPACE                                                                  
RDTWA    NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ASVSTOR                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         SPACE 3                                                                
BUYEL    CLI   0(R6),0                                                          
         BNE   *+8                                                              
         LTR   RE,RE                                                            
         BR    RE                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         BL    BUYEL                                                            
         CLC   0(1,R6),ELCDHI                                                   
         BH    BUYEL                                                            
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
         EJECT                                                                  
*        ERROR ROUTINES                                                         
         SPACE                                                                  
SIZERR   L     R1,=A(SIZMS)                                                     
         LA    R2,TRAMEDH                                                       
         B     COMERR                                                           
         SPACE                                                                  
NOSPTER  L     R1,=A(NOSPTMS)                                                   
         LA    R2,TRAMEDH                                                       
         SPACE                                                                  
COMERR   XC    CONHEAD,CONHEAD                                                  
         A     R1,SPTR54RR                                                      
         BCTR  R1,0                                                             
         ZIC   RF,0(R1)                                                         
         LA    RE,L'CONHEAD                                                     
         CR    RE,RF                                                            
         BNL   *+6                                                              
         DC    H'0'                                                             
         EX    RF,COMERMVC                                                      
         NI    TRAMEDH+4,X'FF'-X'20'                                            
         NI    TRACLTH+4,X'FF'-X'20'                                            
         NI    TRAPRDH+4,X'FF'-X'20'                                            
         NI    TRAMKTH+4,X'FF'-X'20'                                            
         NI    TRAPERH+4,X'FF'-X'20'                                            
ERREXIT  GOTO1 ERREX2                                                           
COMERMVC MVC   CONHEAD(0),1(R1)                                                 
         SPACE 3                                                                
PRDINV   MVI   ERROR,INVPRDCD                                                   
         B     TRAPERR                                                          
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SSPEC H4,3,C'PRODUCT'                                                  
*        SSPEC H5,3,C'DISTRIBUTOR'                                              
         SSPEC H6,3,C'DEALER 1'                                                 
*        SSPEC H6,3,C'DEALER 2'                                                 
         SSPEC H1,39,C'D E A L E R  R E C A P'                                  
         SSPEC H2,38,C'------------------------'                                
         SSPEC H3,38,PERIOD                                                     
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,77,RUN                                                        
         SSPEC H5,77,REQUESTOR                                                  
         SSPEC H5,101,PAGE                                                      
         SSPEC H9,7,C'DATE'                                                     
         SSPEC H10,3,C'-----------'                                             
         SSPEC H9,15,C'DAY'                                                     
         SSPEC H10,15,C'--------'                                               
         SSPEC H9,26,C'TIME'                                                    
         SSPEC H10,26,C'---------'                                              
         SSPEC H9,39,C'PROGRAM'                                                 
         SSPEC H10,39,C'-------'                                                
         SSPEC H9,58,C'DPT'                                                     
         SSPEC H10,58,C'---'                                                    
         SSPEC H9,65,C'LEN'                                                     
         SSPEC H10,65,C'---'                                                    
         SSPEC H9,71,C'COMMERCIAL'                                              
         SSPEC H10,71,C'----------'                                             
         SSPEC H9,84,C'COMMERCIAL NAME'                                         
         SSPEC H10,84,C'---------------'                                        
         SSPEC H9,100,C'STATION'                                                
         SSPEC H10,100,C'-------'                                               
         DC    X'00'               END MARKER FOR SSPEC                         
         SPACE                                                                  
         DC    AL1(L'NOSPTMS-1)                                                 
NOSPTMS  DC    C'* ERROR * NO SPOTS SELECTED *'                                 
         DC    AL1(L'SIZMS-1)                                                   
SIZMS    DC    C'* ERROR * RUN SOON OR DDS, TOO LARGE FOR NOW *'                
         PRINT OFF                                                              
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRDLR                                                        
         TITLE 'T21654 - SPOT RECAP - DSECTS'                                   
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAA4D                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SPTR54RR DS    A                                                                
ASPTABLE DS    A                                                                
ASPTABND DS    A                                                                
ASVSTOR  DS    A                                                                
ATABEND  DS    A                                                                
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
* FROM BUY ELEMENT                                                              
ELDATE   DS    H                                                                
ELDATEX  DS    H                                                                
SPTWORK  DS    CL32                                                             
ROTDAYS  DS    H                                                                
ASVNEXT  DS    A                                                                
ASVCMLS  DS    A                                                                
* FROM PERIOD ENTERED DATES                                                     
GENSTP   DS    H                   PERIOD START DATE                            
GENENDP  DS    H                   PERIOD END DATE                              
SVPERDTS DS    0XL6                                                             
SVPERST  DS    XL3                 START DATE FROM PERIOD HEADING               
SVPEREND DS    XL3                 END DATE FROM PERIOD HEADING                 
SVMKT    DS    XL2                                                              
SVSTA    DS    XL3                                                              
SVTAG    DS    XL2                                                              
SPOTCT   DS    H                                                                
SPOTNUMB DS    XL1                                                              
SPTABLE  DS    XL5590               SPOT TABLE BUILD AREA                       
         SPACE                                                                  
* DSECT FOR SPOT ACTIVITY LIST ENTRIES *                                        
         SPACE                                                                  
SPTABLED DSECT                                                                  
SPTDATA  DS    0XL26                                                            
SPTMKT   DS    XL2                                                              
SPTTAG   DS    XL2                 DEALER TAG ASSIGNED                          
SPTFTD   DS    XL2                 SPOT FIRST TELECAST DATE                     
SPTLTD   DS    XL2                 SPOT LAST TELECAST DATE                      
SPTTIME  DS    XL4                 START/END TIMES                              
SPTSTA   DS    XL3                                                              
SPTSPTN  DS    XL1                 SPOT NUMBER                                  
SPTSLN   DS    XL1                 SPOT LENGTH                                  
SPTDAY   DS    XL1                 DAYS                                         
SPTCMLSQ DS    XL3                 CML SEQ NUMBER                               
SPTDPT   DS    XL1                 DAY PART                                     
SPTDSKAD DS    XL4                 BUY REC DISK ADDRESS                         
         SPACE                                                                  
* DSECT FOR COMMERCIAL SAVE TABLE ENTRY *                                       
         SPACE                                                                  
SVCMLD   DSECT                                                                  
SVCMLDTA DS    0CL26                                                            
SVCMLSEQ DS    XL3                                                              
SVCMLCOD DS    CL8                                                              
SVCMLNM  DS    CL15                                                             
         EJECT                                                                  
* ONLINE LIST                                                                   
         SPACE                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LTAG     DS    CL4                                                              
         DS    CL1                                                              
LDATES   DS    CL11                                                             
         DS    CL1                                                              
LDAYS    DS    CL8                                                              
         DS    CL1                                                              
LTIMES   DS    CL11                                                             
         DS    CL1                                                              
LPNAME   DS    CL17                                                             
         DS    CL1                                                              
LDAYPT   DS    CL1                                                              
         DS    CL1                                                              
LSLN     DS    CL3                                                              
         DS    CL1                                                              
LCML     DS    CL8                                                              
         DS    CL1                                                              
LSTA     DS    CL6                                                              
         SPACE 2                                                                
* DSECT FOR PRINT LINE DATA *                                                   
         SPACE                                                                  
SPOOLD   DSECT                                                                  
*                                                                               
         ORG   P                                                                
*                                                                               
         DS    CL2                                                              
PDATES   DS    CL11                                                             
         DS    CL1                                                              
PDAYS    DS    CL8                                                              
         DS    CL3                                                              
PTIMES   DS    CL7                                                              
         DS    CL6                                                              
PPNAME   DS    CL18                                                             
         DS    CL2                                                              
PDAYPT   DS    CL1                                                              
         DS    CL5                                                              
PSLN     DS    CL3                                                              
         DS    CL4                                                              
PCML     DS    CL8                                                              
         DS    CL4                                                              
PCMLNM   DS    CL15                                                             
         DS    CL1                                                              
PSTA     DS    CL7                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPTRA54   05/01/02'                                      
         END                                                                    

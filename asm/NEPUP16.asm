*          DATA SET NEPUP16    AT LEVEL 051 AS OF 02/13/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 045046.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T32216A                                                                  
         TITLE 'T32216 - PROGRAM COST MAINTENANCE'                              
T32216   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32216**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLC   TWALACT,ACTNUM                                                   
         BE    *+14                                                             
         XC    LASTPKEY,LASTPKEY                                                
         MVI   HEADCH,C'Y'                                                      
         CLC   TWALREC,RECNUM                                                   
         BE    *+14                                                             
         XC    LASTPKEY,LASTPKEY                                                
         MVI   HEADCH,C'Y'                                                      
         SPACE 1                                                                
MODE1    CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         BAS   RE,ALTHEAD                                                       
         GOTO1 READ                                                             
         BAS   RE,MODTWA                                                        
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         CLI   HEADCH,C'Y'                                                      
         BE    MODE3                                                            
         CLC   LASTPKEY,KEY                                                     
         BE    MODE4                                                            
         SPACE 1                                                                
MODE3    MVC   LASTPKEY,KEY                                                     
         MVI   HEADCH,0                                                         
         GOTO1 GETREC                                                           
         BAS   RE,DREC             KEYS ARE DIFFERENT SO DISPLAY                
         MVC   CONHEAD,=CL60'DATA DISPLAYED - ENTER CHANGES'                    
         OI    CONHEADH+6,X'80'                                                 
         LA    R2,PUPUNITH                                                      
         B     XIT2                                                             
         SPACE 1                                                                
MODE4    GOTO1 GETREC                                                           
         BAS   RE,VREC             KEYS ARE THE SAME SO VALIDATE                
         GOTO1 PUTREC                                                           
         BAS   RE,DREC                                                          
         MVC   CONHEAD,=CL60'PROGRAM CHANGED AS REQUESTED'                      
         OI    CONHEADH+6,X'80'                                                 
         LA    R2,PUPCLIH                                                       
         XC    LASTPKEY,LASTPKEY                                                
         B     XIT2                                                             
         EJECT                                                                  
*        CLI   MODE,VALKEY                                                      
*        BNE   MODE2                                                            
*        BAS   RE,VKEY                                                          
*        B     XIT                                                              
*        SPACE 1                                                                
*MODE2   CLI   MODE,VALREC                                                      
*        BNE   MODE4                                                            
*        BAS   RE,VKEY                                                          
*        BAS   RE,VREC                                                          
*        BAS   RE,DREC                                                          
*        XC    DMCB+12(4),DMCB+12                                               
*        B     XIT                                                              
*        SPACE 1                                                                
*MODE4   CLI   MODE,DISPKEY                                                     
*        BNE   MODE6                                                            
*        BAS   RE,DKEY                                                          
*        B     XIT                                                              
*        SPACE 1                                                                
*MODE6   CLI   MODE,DISPREC                                                     
*        BNE   MODE8                                                            
*        BAS   RE,DREC                                                          
*        B     XIT                                                              
*        SPACE 1                                                                
*MODE8   CLI   MODE,RECPUT                                                      
*        BNE   XIT                                                              
*        MVC   AIO,AIO2                                                         
*        LA    R4,KEY                                                           
*        AH    R4,LKEY                                                          
*        AH    R4,LSTATUS                                                       
*        MVC   0(4,R4),GLOBDA                                                   
*        GOTO1 GETREC                                                           
*        MVC   AIO,AIO1                                                         
*        B     XIT                                                              
*        EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
         SPACE 3                                                                
VKEY     NTR1                                                                   
         LA    R2,PUPCLIH          CLIENT                                       
         GOTO1 VVALCLT                                                          
         SPACE 1                                                                
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         SPACE 1                                                                
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         MVI   PROGPERQ,0                                                       
         CLI   PLANPERT,C'W'                                                    
         BNE   VKEY2                                                            
         LA    R2,PUPQURTH         WEEKLIES NEED QUARTER NUMBER                 
*                                                                               
         CLI   5(R2),0                                                          
         BE    BADQUART                                                         
         CLI   8(R2),C'1'          (1-4)                                        
         BL    BADQUART                                                         
         CLI   8(R2),C'4'                                                       
         BH    BADQUART                                                         
         ZIC   R1,8(R2)                                                         
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         STC   R1,PROGPERQ                                                      
         CLI   PROGPERQ,4          (FOURTH IS QUARTER 0)                        
         BNE   *+8                                                              
         MVI   PROGPERQ,0                                                       
         MVC   BACKOPT,9(R2)       (OPTION TO BACKUP FOR WEEKLIES)              
         SPACE 1                                                                
VKEY2    LA    R2,PUPPROGH         PROGRAM                                      
*                                                                               
         GOTO1 ANY                                                              
         MVC   PROGCODE,WORK                                                    
         ZIC   R1,PLANYEAR         USE START YEAR                               
         BCTR  R1,0                                                             
         STC   R1,PERIOD                                                        
*        MVI   PERIOD+1,3          SET PERIOD FOR 3RD QUARTER                   
         MVI   PERIOD+1,4          SET PERIOD FOR 4TH QUARTER                   
         CLI   PLANPERT,C'M'                                                    
         BNE   *+8                                                              
         MVI   PERIOD+1,9          OR SEPTEMBER IF MONTHLY                      
         CLI   PLANPERT,C'W'       USE FIRST WEEK FOR WEEKLIES                  
         BNE   *+10                                                             
         MVC   PERIOD,CMONDAYS                                                  
         GOTO1 VLUPPROG                                                         
         MVC   PROGCODE,WORK                                                    
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPUKEY,R4                                                        
         MVI   NPUKTYPE,X'22'      FILL PROGRAM KEY                             
         MVC   NPUKAM,BINAGYMD                                                  
         MVC   NPUKCLT,CLTCOMP                                                  
         MVC   NPUKNET,NETWORK                                                  
         MVC   NPUKDPT,DPTCODE                                                  
         MVC   NPUKPLAN,PLANCODE                                                
         MVC   NPUKPROG,PROGCODE                                                
         MVC   NPUKPERQ,PROGPERQ                                                
         BAS   RE,SHOWPROG                                                      
         OI    PUPPROGH+6,X'80'                                                 
         B     XIT                                                              
*        EJECT                                                                  
*              SEE IF HEADLINES CHANGED                                         
         SPACE 3                                                                
ALTHEAD  NTR1                                                                   
         LA    R2,PUPCLIH          CLIENT                                       
         TM    4(R2),X'80'         WAS FIELD CHANGED                            
         BZ    *+12                                                             
         MVI   HEADCH,C'Y'                                                      
         NI    4(R2),X'7F'         FIELD PREVIOUSLY VALIDATED                   
*                                                                               
         LA    R2,PUPNETH          NETWORK                                      
         TM    4(R2),X'80'         WAS FIELD CHANGED                            
         BZ    *+12                                                             
         MVI   HEADCH,C'Y'                                                      
         NI    4(R2),X'7F'         FIELD PREVIOUSLY VALIDATED                   
*                                                                               
         LA    R2,PUPDPTH          DAYPART                                      
         TM    4(R2),X'80'         WAS FIELD CHANGED                            
         BZ    *+12                                                             
         MVI   HEADCH,C'Y'                                                      
         NI    4(R2),X'7F'         FIELD PREVIOUSLY VALIDATED                   
*                                                                               
         LA    R2,PUPPLANH         PLAN                                         
         TM    4(R2),X'80'         WAS FIELD CHANGED                            
         BZ    *+12                                                             
         MVI   HEADCH,C'Y'                                                      
         NI    4(R2),X'7F'         FIELD PREVIOUSLY VALIDATED                   
*                                                                               
         LA    R2,PUPQURTH         QUARTER                                      
         TM    4(R2),X'80'         NEW FIELD                                    
         BZ    *+12                                                             
         MVI   HEADCH,C'Y'                                                      
         NI    4(R2),X'7F'         ESET NEW FIELD BIT                           
*                                                                               
         LA    R2,PUPPROGH         PROGRAM                                      
         TM    4(R2),X'80'         WAS FIELD CHANGED                            
         BZ    *+12                                                             
         MVI   HEADCH,C'Y'                                                      
         NI    4(R2),X'7F'         FIELD PREVIOUSLY VALIDATED                   
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         L     RE,AIO                                                           
         LA    RF,2000                                                          
         XCEF                                                                   
         L     R1,AIO                                                           
         MVC   0(20,R1),KEY        ENSURE KEY IS OK IN RECORD                   
         CLI   ACTNUM,ACTADD                                                    
         BE    VREC1                                                            
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         BAS   RE,TRIMREC                                                       
         SPACE 1                                                                
VREC1    LA    R6,ELEMENT                                                       
         USING NPGDEL,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   NPGDEL,X'01'                                                     
         MVI   NPGDLEN,32                                                       
         MVC   NPGDNAME,PROGNAME                                                
         MVC   NPGDDAY,PROGDAYC                                                 
         MVC   NPGDTIME,PROGMIL                                                 
         MVC   NPGDFILT,PROGFILT                                                
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
         MVC   ELEMENT(7),PBEL     BOOK ELEMENT                                 
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
         LA    R2,PUPUNITH         SET UP FOR ASSIGNMENT RECORDS                
         LA    R3,QPERLIST                                                      
         LA    R4,4                                                             
         CLI   PLANPERT,C'Q'                                                    
         BE    VREC2                                                            
         LA    R3,MPERLIST                                                      
         LA    R4,13                                                            
         CLI   PLANPERT,C'M'                                                    
         BE    VREC2                                                            
         LA    R3,CMONDAYS                                                      
         SPACE 1                                                                
VREC2    BAS   RE,VALALINE                                                      
         LA    R0,9                                                             
         BAS   RE,BUMPN                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,VREC2                                                         
         L     R4,AIO              ENSURE KEY AGREES WITH IO                    
         MVC   KEY,0(R4)                                                        
         B     XIT                                                              
         SPACE 1                                                                
QPERLIST DC    AL1(4,1,1,2,2,2,3,2)                                             
MPERLIST DC    AL1(9,1,10,1,11,1,12,1,1,2,2,2)                                  
         DC    AL1(3,2,4,2,5,2,6,2,7,2,8,2,9,2)                                 
         EJECT                                                                  
*              ROUTINE TO TRIM DOWN PROGRAM RECORDS                             
         SPACE 3                                                                
TRIMREC  NTR1                                                                   
         MVI   ELCODE,0                                                         
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     TRIM4                                                            
         SPACE 1                                                                
TRIM2    BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
TRIM4    BNE   TRIMEND                                                          
         CLI   0(R6),X'12'         NEED TO SAVE NEW UNITS                       
         BE    TRIM2                                                            
         CLI   0(R6),X'14'         NEED TO SAVE VPH                             
         BE    TRIM2                                                            
         SPACE 1                                                                
TRIMNO   MVI   0(R6),X'01'                                                      
         B     TRIM2                                                            
         SPACE 1                                                                
TRIMEND  MVI   ELCODE,X'01'        NOW GET RID OF UNWANTED ELEMENTS             
         GOTO1 REMELEM                                                          
         L     R6,AIO                                                           
         LA    R6,25(R6)                                                        
         LA    R1,26                                                            
         SPACE 1                                                                
TRIMEND2 CLI   0(R6),0                                                          
         BE    TRIMEND4                                                         
         ZIC   R0,1(R6)                                                         
         AR    R1,R0                                                            
         AR    R6,R0                                                            
         B     TRIMEND2                                                         
         SPACE 1                                                                
TRIMEND4 L     R6,AIO                                                           
         STH   R1,20(R6)           REFRESH RECORD LENGTH!                       
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE A LINE OF INPUT                                         
         SPACE 3                                                                
*              INPUTS              R2=A(FIRST LENGTH INPUT)                     
*                                  R3=A(Q/YEAR OR MONTH/YEAR OR PERIOD)         
         SPACE 1                                                                
VALALINE NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING NPUBD,R6                                                         
         MVI   NPUBEL,X'12'                                                     
         MVI   NPUBLEN,56                                                       
         ZIC   R1,PLANYEAR                                                      
         CLI   1(R3),1                                                          
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         STC   R1,NPUBPER          SAVE YEAR                                    
         MVC   NPUBPER+1(1),0(R3)  AND QUARTER OR MONTH                         
         CLI   PLANPERT,C'W'                                                    
         BNE   *+10                                                             
         MVC   NPUBPER,0(R3)       WEEKLY SAVES ACTUAL DATE                     
         MVC   PERIOD,NPUBPER                                                   
         LA    R3,NPUBUNS                                                       
         LA    R4,4                                                             
         LR    R5,R2                                                            
         LA    R0,4                                                             
         BAS   RE,BUMP5N                                                        
         SPACE 1                                                                
VALL2    CLI   5(R2),0             CHECK INPUT OF UNITS                         
         BNE   VALL3                                                            
         CLI   5(R5),0             NO UNITS NO COST ALLOWED                     
         BE    VALL4                                                            
         MVI   5(R5),0             ZERO OUT COST                                
         XC    8(11,R5),8(R5)                                                   
         B     VALL4                                                            
*                                                                               
VALL3    GOTO1 VALINUM             MUST THEM BE NUMERIC                         
         MVC   0(1,R3),ACTUAL                                                   
         SPACE 1                                                                
VALL4    BAS   RE,BUMP                                                          
         BAS   RE,BUMP5                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,VALL2                                                         
         OC    NPUBUNS,NPUBUNS     ANY UNITS INPUT?                             
         BZ    CHKDEL              NO - DON'T NEED THIS LINE                    
         GOTO1 VLUPPROG                                                         
         GOTO1 VLUPHUT                                                          
*                                                                               
         XC    NPUBAMT,NPUBAMT                                                  
         LA    R3,NPUBAMT                                                       
         LA    R4,4                                                             
         SPACE 1                                                                
VALL10   CLI   5(R2),0             PICK UP L'DECIMAL FIELD                      
         BE    VALL20                                                           
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,8(R2)),(R0)                                      
         CLI   0(R1),X'FF'         CHECK FOR ERROR                              
         BE    BADCOST                                                          
         MVC   0(4,R3),DMCB+4                                                   
         SPACE 1                                                                
VALL20   BAS   RE,BUMP                                                          
         LA    R3,4(R3)                                                         
         BCT   R4,VALL10                                                        
*                                                                               
         MVI   ELCODE,X'12'        NOW SEE IF ELEMENT THERE ALREADY             
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     VALL50                                                           
         SPACE 1                                                                
VALL40   BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
VALL50   BNE   VALLNO                                                           
         CLC   ELEMENT+2(2),2(R6)  CHECK FOR SAME PERIOD                        
         BNE   VALL40                                                           
         MVC   4(4,R6),ELEMENT+4    OVERWRITE THE NEMBER OF UNITS               
         MVC   38(16,R6),ELEMENT+38 OVERWRITE THE COST FIELD                    
         B     XIT                                                              
         SPACE 1                                                                
VALLNO   GOTO1 ADDELEM                                                          
         B     XIT                                                              
*                                                                               
CHKDEL   DS    0H                                                               
         MVI   ELCODE,X'12'        IF ELEMENT THERE ALREADY                     
         L     R6,AIO              DELETE IT                                    
         BAS   RE,GETEL                                                         
         B     CDL10                                                            
         SPACE 1                                                                
CDL8     BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
CDL10    BNE   CDLX                                                             
         CLC   ELEMENT+2(2),2(R6)  CHECK FOR SAME PERIOD                        
         BNE   CDL8                                                             
         LA    R3,ELEMENT+2                                                     
         GOTO1 HELLO,DMCB,(C'D',=C'UNTFILE '),(X'12',AIO),(2,0(R3))             
         SPACE 1                                                                
CDLX     B     XIT                                                              
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 3                                                                
DKEY     NTR1                                                                   
         L     R4,AIO                                                           
         USING NPURECD,R4                                                       
         GOTO1 CLUNPK,DMCB,NPUKCLT,PUPCLI                                       
         MVC   CLTCOMP,NPUKCLT                                                  
         LA    R2,PUPCLIH                                                       
         MVC   AIO,AIO2                                                         
         MVC   SAVPRKEY,KEY                                                     
         GOTO1 VVALCLT                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVPRKEY                                                     
         MVC   PUPNET(4),NPUKNET                                                
         OI    PUPNETH+6,X'80'                                                  
         MVC   NETWORK,PUPNET                                                   
         MVI   NETMAIN,C'Y'                                                     
         CLC   NETWORK(3),=C'ABC'                                               
         BE    DKEY2                                                            
         CLC   NETWORK(3),=C'CBS'                                               
         BE    DKEY2                                                            
         CLC   NETWORK(3),=C'NBC'                                               
         BE    DKEY2                                                            
         MVI   NETMAIN,C'N'                                                     
         SPACE 1                                                                
DKEY2    DS    0H                                                               
         LA    R2,PUPNETH                                                       
         MVC   AIO,AIO2                                                         
         MVC   SAVPRKEY,KEY                                                     
         GOTO1 VVALNET                                                          
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVPRKEY                                                     
*                                                                               
         MVC   WORK(1),NPUKDPT                                                  
         MVC   DPTCODE,WORK                                                     
         GOTO1 VLUPDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         MVC   PUPPLAN,NPUKPLAN                                                 
         MVC   PLANCODE,PUPPLAN                                                 
         OI    PUPPLANH+6,X'80'                                                 
         LA    R2,PUPPLANH                                                      
         MVI   5(R2),4                                                          
         MVC   AIO,AIO2                                                         
         MVC   SAVPRKEY,KEY                                                     
         GOTO1 VVALPLAN                                                         
         MVC   AIO,AIO1                                                         
         MVC   KEY,SAVPRKEY                                                     
         MVC   PUPPROG(6),NPUKPROG                                              
         OI    PUPPROGH+6,X'80'                                                 
         MVC   PROGPERQ,NPUKPERQ                                                
         XC    PUPQURT,PUPQURT                                                  
         OI    PUPQURTH+6,X'80'                                                 
         CLI   PLANPERT,C'W'                                                    
         BNE   DKEY4                                                            
         MVC   PUPQURT(1),NPUKPERQ                                              
         OI    PUPQURT,X'F0'                                                    
         CLI   NPUKPERQ,0                                                       
         BNE   *+8                                                              
         MVI   PUPQURT,C'4'                                                     
         MVI   BACKOPT,0                                                        
         CLI   THISLARG,C'-'       CAN PASS - FROM LIST                         
         BNE   DKEY4                                                            
         MVI   PUPQURT+1,C'-'                                                   
         MVI   BACKOPT,C'-'                                                     
         SPACE 1                                                                
DKEY4    BAS   RE,MODTWA                                                        
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 3                                                                
DREC     NTR1                                                                   
         SPACE 1                                                                
         GOTO1 VEXTPROG                                                         
         BAS   RE,SHOWPROG                                                      
         XC    BUFF(64),BUFF                                                    
         LA    R5,BUFF                                                          
         USING BUFFD,R5                                                         
         LA    R2,PUPUNITH                                                      
         CLI   PLANPERT,C'W'       IF WE ARE DOING WEEKS                        
         BNE   DREC1                                                            
         LA    R3,OMONDAYS         DEAL WITH 'OTHER' WEEKS FIRST                
         LA    R4,3                                                             
         SPACE 1                                                                
DOTH2    BAS   RE,DLINE            THIS IS SO OTHERS ADD INTO TOTALS            
         LA    R3,2(R3)                                                         
         BCT   R4,DOTH2                                                         
         MVI   HIDDEN,C'N'         TAKE A NOTE IF THERE WERE ANY                
         OC    TOTUNS(16),TOTUNS                                                
         BZ    DREC1                                                            
         MVI   HIDDEN,C'Y'                                                      
         SPACE 1                                                                
DREC1    LA    R3,QPERLIST                                                      
         LA    R4,4                                                             
         CLI   PLANPERT,C'Q'                                                    
         BE    DREC2                                                            
         LA    R3,MPERLIST                                                      
         LA    R4,13                                                            
         CLI   PLANPERT,C'M'                                                    
         BE    DREC2                                                            
         LA    R3,CMONDAYS                                                      
         SPACE 1                                                                
DREC2    BAS   RE,DLINE                                                         
         LA    R0,9                                                             
         BAS   RE,BUMPN                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,DREC2                                                         
         BAS   RE,DTOTAL                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS A LINE OF FIELDS                                
         SPACE 3                                                                
*              INPUTS              R2=A(FIRST UNIT FIELD ON LINE)               
*                                  R3=A(Q/YEAR OR MONTH/Y OR PERIOD)            
*                                  R5=A(BUFFD - ACCUMS ETC)                     
         SPACE 1                                                                
DLINE    NTR1                                                                   
         L     R4,AIO                                                           
         USING NPURECD,R4                                                       
         ZIC   R1,PLANYEAR         SET PERIOD FOR THIS LINE                     
         CLI   1(R3),1                                                          
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         STC   R1,PERIOD                                                        
         MVC   PERIOD+1(1),0(R3)                                                
         CLI   PLANPERT,C'W'                                                    
         BNE   *+10                                                             
         MVC   PERIOD,0(R3)        WEEKLY USES ACTUAL DATE                      
         LA    R6,NPGDEL                                                        
         DROP  R4                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,0                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
DLINE2   BAS   RE,NEXTEL                                                        
         BNE   DLINE4                                                           
         CLI   0(R6),X'02'         LOOKING FOR OLD OR NEW STYLE                 
         BE    DLINE3              (THE DSECT IS THE SAME)                      
         CLI   0(R6),X'12'                                                      
         BNE   DLINE2                                                           
         USING NPUBD,R6                                                         
DLINE3   CLC   NPUBPER,PERIOD      LOOK FOR MATCH ON PERIOD                     
         BE    DLINE6                                                           
         B     DLINE2                                                           
         SPACE 1                                                                
DLINE4   XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         SPACE 1                                                                
DLINE6   LA    R3,NPUBUNS          DISPLAY THE UNITS                            
         SR    R4,R4                                                            
         LA    RE,TOTUNS                                                        
         LA    RF,4                                                             
         SPACE 1                                                                
DLINE8   EDIT  (1,0(R3)),(3,8(R2)),ALIGN=LEFT                                   
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R3)                                                         
         AR    R4,R1               ADD INTO UNITS FOR PERIOD                    
         A     R1,0(RE)            ADD INTO TOTAL FOR LENGTH                    
         ST    R1,0(RE)                                                         
         SPACE 1                                                                
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    R3,1(R3)                                                         
         LA    RE,4(RE)                                                         
         BCT   RF,DLINE8                                                        
         SPACE 1                                                                
DLINE20  LA    R3,NPUBAMT                                                       
         LA    R4,TOTDLRS                                                       
         LA    R5,NPUBUNS          UNIT COUNTS                                  
         LA    RF,4                                                             
         SPACE 1                                                                
DLINE30  BAS   RE,DISCOST                                                       
         OI    6(R2),X'80'                                                      
         ZIC   R6,0(R5)            LOAD NUMBER OF UNITS INTO R6                 
         L     R1,0(R3)            LOAD AMOUNT INTO R1                          
         SR    R0,R0                                                            
         MR    R0,R6               UNITS X ACTUALS                              
         A     R1,0(R4)            ADD INTO TOTAL FOR LENGTH                    
         ST    R1,0(R4)                                                         
         SPACE 1                                                                
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         LA    R3,4(R3)                                                         
         LA    R5,1(R5)                                                         
         LA    R4,4(R4)                                                         
         BCT   RF,DLINE30                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS TOTAL LINE                                      
         SPACE 3                                                                
DTOTAL   NTR1                                                                   
         LA    R2,PUPPERH                                                       
         SPACE 1                                                                
DTOTAL2  CLC   8(5,R2),=C'TOTAL'                                                
         BE    DTOTAL4                                                          
         BAS   RE,BUMP                                                          
         B     DTOTAL2                                                          
         SPACE 1                                                                
DTOTAL4  BAS   RE,BUMP                                                          
         LA    R3,TOTUNS                                                        
         LA    R4,4                                                             
         SPACE 1                                                                
DTOTAL6  EDIT  (4,0(R3)),(3,8(R2)),ALIGN=LEFT                                   
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         LA    R3,4(R3)                                                         
         BCT   R4,DTOTAL6                                                       
*                                                                               
         LA    R3,TOTDLRS                                                       
         LA    R4,4                                                             
         SPACE 1                                                                
DTOTAL10 BAS   RE,DISCOST                                                       
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         LA    R3,4(R3)                                                         
         BCT   R4,DTOTAL10                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS THE COST FIELD                                  
*              INPUT  R3 = A(COST)                                              
         SPACE                                                                  
DISCOST  NTR1                                                                   
         L     R4,0(R3)                                                         
         LR    R0,R4               SAVE COST VALUE                              
         SRDA  R4,32               PREPARE DIVIDEND                             
         D     R4,=F'100'          SEPARATE DOLLARS AND PENNIES                 
         LTR   R4,R4               TEST REMAINDER (PENNIES)                     
         BNZ   DISCST2             YES                                          
         EDIT  (R5),(11,8(R2)),ALIGN=LEFT,MINUS=YES                             
         B     DISCSTEX                                                         
         SPACE                                                                  
DISCST2  LR    R4,R0               RESTORE COST VALUE W PENNIES                 
         EDIT  (R4),(11,8(R2)),2,ALIGN=LEFT,MINUS=YES                           
*                                                                               
DISCSTEX B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MODIFY TWA FOR PLAN                                   
         SPACE 3                                                                
MODTWA   NTR1                                                                   
         SPACE 1                                                                
         LA    R2,PUPLENS          SHOW LENGTHS IN HEADINGS                     
         MVC   PUPLENS,SPACES                                                   
         OI    PUPLENSH+6,X'80'                                                 
         LA    R3,PLANLENS                                                      
         ZIC   R4,PLANNLEN                                                      
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
MODTWA4  EDIT  (1,0(R3)),(3,0(R2))                                              
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         MVI   0(R2),C':'          DISPLAYED AS :NN                             
         LA    R2,5(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,MODTWA4                                                       
         SPACE 1                                                                
MODTWA8  LA    R2,PUPAMTS          SHOW LENGTHS IN HEADINGS                     
         MVC   PUPAMTS,SPACES                                                   
         OI    PUPAMTSH+6,X'80'                                                 
         LA    R3,PLANLENS                                                      
         ZIC   R4,PLANNLEN                                                      
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
MODTWA10 MVC   0(5,R2),=CL5'COST/'                                              
         EDIT  (1,0(R3)),(3,5(R2)),ALIGN=LEFT                                   
         LA    R2,13(R2)                                                        
         LA    R3,1(R3)                                                         
         BCT   R4,MODTWA10                                                      
         SPACE 1                                                                
         LA    R2,PUPPERH                                                       
         LA    R3,QLIST                                                         
         LA    R4,14                                                            
         CLI   PLANPERT,C'Q'                                                    
         BE    MODTWA16                                                         
         LA    R3,MLIST                                                         
         CLI   PLANPERT,C'M'                                                    
         BE    MODTWA16                                                         
         BAS   RE,WEEKDATS                                                      
         LA    R3,OVWORK+15                                                     
         SPACE 1                                                                
MODTWA16 BAS   RE,MODALINE                                                      
         LA    R0,9                                                             
         BAS   RE,BUMPN            (SKIP PAST 7 FIELDS)                         
         LA    R3,5(R3)                                                         
         BCT   R4,MODTWA16                                                      
         MVI   PUPSWCH,X'C1'                                                    
         MVI   PUPSWCHH+5,1                                                     
         MVC   PUPHIDE+10(1),PLANNLEN                                           
         MVC   PUPHIDE+11(1),PLANYEAR                                           
         MVC   PUPHIDE+12(1),PLANPERT                                           
         MVC   LASTQURT,PUPQURT                                                 
         B     XIT                                                              
         SPACE 1                                                                
MLIST    DC    C'SEP01OCT01NOV01DEC01JAN02FEB02MAR02'                           
         DC    C'APR02MAY02JUN02JUL02AUG02SEP02TOTAL'                           
         SPACE 1                                                                
QLIST    DC    C'Q4/01Q1/02Q2/02Q3/02     TOTAL     '                           
         DC    C'                                   '                           
         EJECT                                                                  
*              ROUTINE TO CONSTRUCT SCREEN LINE                                 
         SPACE 3                                                                
*                                  R2=A(PERIOD FIELD HEADER)                    
*                                  R3=A(5 BYTE LIST EXPRESSION)                 
         SPACE 1                                                                
MODALINE NTR1                                                                   
         MVC   8(5,R2),0(R3)       MMMYY OR QN/YY OR TOTAL                      
         OI    6(R2),X'80'                                                      
         SR    R4,R4               R4 SET TO ZERO                               
         CLI   0(R3),C' '                                                       
         BE    MODL2                                                            
         CLC   0(5,R3),=C'TOTAL'                                                
         BE    MODL2                                                            
         ZIC   R4,PLANNLEN                OR N'LENS                             
         CLI   PLANPERT,C'W'                                                    
         BE    MODL2                                                            
         ZIC   R1,PLANYEAR         USE PLAN YEAR                                
         CLI   4(R3),C'1'                                                       
         BNE   *+6                                                              
         BCTR  R1,0                OR 1 LESS                                    
         EDIT  (R1),(2,11(R2))                                                  
         SPACE 1                                                                
MODL2    BAS   RE,BUMP                                                          
         CLI   PUPSWCH,X'C1'       SEE IF RECORD TYPE CHANGED                   
         BNE   MODL3                                                            
         CLC   PLANNLEN,PUPHIDE+10 MODIFY FIELDS IF N'LENGTHS                   
         BNE   MODL3                                                            
         CLC   LASTQURT(1),PUPQURT OR QUARTER                                   
         BNE   MODL3                                                            
         CLC   PLANPERT,PUPHIDE+12 OR PERIOD TYPE HAVE CHANGED                  
         BE    XIT                                                              
         SPACE 1                                                                
MODL3    MH    R4,=H'8'                                                         
         LA    R4,PROTLIST(R4)                                                  
         LA    R0,8                                                             
         SPACE 1                                                                
MODL4    OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         MVI   8(R2),C' '                                                       
         SH    R1,=H'10'                                                        
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   9(0,R2),8(R2)       CLEAR FIELD                                  
         NI    1(R2),X'DF'         MAKE UNPROTECTED                             
         CLI   0(R4),C'Y'          UNLESS FIELD IS PROTECTED                    
         BNE   *+8                                                              
         OI    1(R2),X'20'                                                      
         MVI   5(R2),0             AND CLEAR INPUT LENGTH                       
         BAS   RE,BUMP                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,MODL4                                                         
         B     XIT                                                              
         SPACE 1                                                                
PROTLIST DC    C'YYYYYYYY'                                                      
         DC    C'NYYYNYYY'                                                      
         DC    C'NNYYNNYY'                                                      
         DC    C'NNNYNNNY'                                                      
         DC    C'NNNNNNNN'                                                      
         EJECT                                                                  
*              ROUTINE TO SHOW PROGRAM DETAILS ON SCREEN                        
         SPACE 3                                                                
SHOWPROG NTR1                                                                   
         MVC   PUPPNAM,PROGNAME                                                 
         OI    PUPPNAMH+6,X'80'                                                 
         SPACE 1                                                                
         MVC   PUPPDT(3),PROGDAY                                                
         MVC   PUPPDT+4(11),PROGTIME                                            
         OI    PUPPDTH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET WEEK START DATES                                  
*              USED FOR WEEKLY PLANS                                            
         SPACE 3                                                                
WEEKDATS NTR1                                                                   
         ZIC   R1,PROGPERQ         GET END OF QUARTER FROM QUARTER NO.          
         SLL   R1,2                                                             
         LA    R1,QENDDATE(R1)                                                  
         MVC   WORK+2(4),0(R1)                MMDD FROM TABLE                   
         ZIC   R2,PLANYEAR         PLAN YEAR                                    
         CLI   PROGPERQ,0                                                       
         BNE   *+6                                                              
         BCTR  R2,0                OR PREVIOUS FOR FOURTH QUARTER               
         EDIT  (R2),(2,WORK),WRK=DMCB,FILL=0                                    
         SPACE 1                                                                
         GOTO1 GETDAY,DMCB,WORK,WORK+6       FIND PREVIOUS SUNDAY               
         ZIC   R0,DMCB             (END OF QUARTER DAY IN R0)                   
         ZIC   R1,ZEROPROF+8       PICK UP PROFILE START OF WEEK                
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                (DEFAULT IS MONDAY)                          
         BCT   R1,*+8              BACK UP TO END OF WEEK                       
         LA    R1,7                MONDAY BACKS UP TO SUNDAY                    
         SR    R1,R0                                                            
         BZ    WKD2                                                             
         BM    *+8                                                              
WKD1     SH    R1,=H'7'                                                         
         CLI   N0PROF+3,C'C'       IF WE ARE USING CALENDAR                     
         BNE   *+8                    ADD IN ANOTHER WEEK                       
         AH    R1,=H'7'                                                         
         ST    R1,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         MVC   WORK(6),WORK+6                                                   
         SPACE 1                                                                
*-- NEXT FOUR LINES ARE NEEDED TO CORRECT A FLAW IN THE 1995                    
*-- YEAR DATE CALCULATIONS, DEALING WITH THE THIRD QUARTER.                     
WKD2     CLC   WORK(6),=CL6'951001'                                             
         BNE   *+12                                                             
         CLI   N0PROF+3,C'B'       IF WE ARE USING BROADCAST                    
         BE    WKD1                   ADD IN ANOTHER WEEK                       
*                                                                               
         L     R1,=F'-111'         BACK UP 16 WEEKS TO FIRST MONDAY             
         ST    R1,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         LA    R2,OVWORK                                                        
         LA    R3,CMONDAYS                                                      
         LA    R0,16                                                            
         CLI   BACKOPT,C'-'                                                     
         BNE   WKD4                                                             
         LA    R2,OVWORK+15                                                     
         SPACE 1                                                                
WKD4     MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(7,0(R2))                                   
         GOTO1 DATCON,DMCB,(0,WORK),(2,0(R3))                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'7'                                      
         LA    R2,5(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,WKD4                                                          
         MVC   OVWORK+80(5),=C'TOTAL'                                           
         CLI   BACKOPT,C'-'        IF WE ARE NOT BACKING UP                     
         BE    XIT                                                              
         MVC   WORK(6),CMONDAYS    SAVE FIRST THREE MONDAYS                     
         MVC   CMONDAYS,CMONDAYS+6 ACTIVE LIST IS THREE WEEKS ON                
         MVC   OMONDAYS,WORK       AND OTHER LIST IS FIRST THREE                
         B     XIT                                                              
         SPACE 1                                                                
QENDDATE DC    C'1231033106301001'    FOUR MMDD PAIRS                           
         EJECT                                                                  
*              ERROR EXITS                                                      
         SPACE 3                                                                
BADCOST  MVC   CONHEAD(L'COSTERR),COSTERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADUNCT  LR    R2,R5                                                            
         MVC   CONHEAD(L'UNCTERR),UNCTERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADUNIT  MVC   CONHEAD(L'UNITERR),UNITERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADQUART MVC   CONHEAD(L'QURTERR),QURTERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
COSTERR  DC    C'** ERROR ** INVALID COST EXPRESSION'                           
UNCTERR  DC    C'** ERROR ** COST NOT ALLOWED WITH ZERO UNITS'                  
UNITERR  DC    C'** ERROR ** NEED NUMBER OF UNITS'                              
QURTERR  DC    C'** ERROR ** NEED QUARTER NUMBER (1-4) FOR WEEKLIES'            
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMP5    ZIC   RF,0(R5)                                                         
         AR    R5,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPN    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,BUMPN                                                         
         BR    RE                                                               
         SPACE 1                                                                
BUMP5N   ZIC   RF,0(R5)                                                         
         AR    R5,RF                                                            
         BCT   R0,BUMP5N                                                        
         BR    RE                                                               
         SPACE 3                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
XIT      CLI   PUPHIDE,C' '                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         SPACE 2                                                                
XIT2     CLI   PUPHIDE,C' '                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR MODULE STORAGE                                         
         SPACE 3                                                                
BUFFD    DSECT                                                                  
TOTUNS   DS    4F                  UNITS FOR LENGTHS 1-4                        
TOTDLRS  DS    4F                  DOLLARS FOR LENGTHS 1-4                      
         DS    CL32                SPARE                                        
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPE7D                                                       
         ORG   CONTAGH+2600                                                     
         SPACE 1                                                                
PUPHIDE  DS    CL16                                                             
MODSW    DS    CL1                                                              
SAVPRKEY DS    CL32                                                             
CMONDAYS DS    CL26                MONDAYS ON THE SCREEN                        
OMONDAYS DS    CL6                 HIDDEN MONDAYS                               
BACKOPT  DS    CL1                                                              
VPHLINK  DS    XL1                                                              
LASTQURT DS    XL2                                                              
HIDDEN   DS    CL1                                                              
LASTPKEY DS    CL20                                                             
HEADCH   DS    CL1                 Y=CHANGE IN HEADLINES                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051NEPUP16   02/13/15'                                      
         END                                                                    

*          DATA SET NEPUP11    AT LEVEL 016 AS OF 05/01/02                      
*          DATA SET NEPUP11    AT LEVEL 057 AS OF 03/08/00                      
*          DATA SET NEPUP11    AT LEVEL 029 AS OF 06/18/90                      
*PHASE T32211A,*                                                                
         TITLE 'T32211 - PROGRAM MAINTENANCE'                                   
T32211   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32211**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VKEY                                                          
         BAS   RE,VREC                                                          
         BAS   RE,DREC                                                          
         XC    DMCB+12(4),DMCB+12                                               
         B     XIT                                                              
         SPACE 1                                                                
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE6    CLI   MODE,DISPREC                                                     
         BNE   MODE8                                                            
         BAS   RE,DREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE8    CLI   MODE,RECPUT                                                      
         BNE   XIT                                                              
         MVC   AIO,AIO2                                                         
         LA    R4,KEY                                                           
         AH    R4,LKEY                                                          
         AH    R4,LSTATUS                                                       
         MVC   0(4,R4),GLOBDA                                                   
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
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
VKEY2    BAS   RE,MODTWA                                                        
         BAS   RE,SHOWPLAN                                                      
         SPACE 1                                                                
         LA    R2,PUPPROGH         PROGRAM                                      
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
         CLI   PLANPRCB,C'Y'                                                    
         BNE   VKEY6                                                            
         GOTO1 VLUPCPRG            GET CABLE PROGARM INFO                       
         B     VKEY20                                                           
VKEY6    GOTO1 VLUPPROG                                                         
VKEY20   MVC   PROGCODE,WORK                                                    
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
         MVC   NPGDNTI,PROGNTI                                                  
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
         LA    R0,7                                                             
         BAS   RE,BUMPN                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,VREC2                                                         
         L     R4,AIO              ENSURE KEY AGREES WITH IO                    
         MVC   KEY,0(R4)                                                        
*                                                                               
         TM    WHENOK,X'01'        IS THIS SPECIAL STEREO ADD                   
         BZ    XIT                 NO XIT                                       
         BAS   RE,UPDREC                                                        
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
         CLI   0(R6),X'14'         NEED TO SAVE NEW UNITS VPH ELEMS             
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
         SPACE 1                                                                
VALL2    CLI   PLANYEAR,101        FUDGE: IF 2001                               
         BNE   SKPFDGE                                                          
         CLC   NPUBPER,=X'CA7A'    AND MAR26/01                                 
         BNE   SKPFDGE                                                          
         CLI   PROGPERQ,1          AND 1ST QUARTER                              
         BNE   SKPFDGE                                                          
         CLI   N0PROF+3,C'B'       AND BROADCAST                                
         BNE   SKPFDGE                                                          
         B     CHKDEL              IGNORE ANY INPUT                             
*                                                                               
SKPFDGE  CLI   5(R2),0             CHECK INPUT OF UNITS                         
         BE    VALL4                                                            
         GOTO1 VALINUM             MUST THEM BE NUMERIC                         
         MVC   0(1,R3),ACTUAL                                                   
         SPACE 1                                                                
VALL4    BAS   RE,BUMP                                                          
         LA    R3,1(R3)                                                         
         BCT   R4,VALL2                                                         
         OC    NPUBUNS,NPUBUNS     ANY UNITS INPUT?                             
         BZ    CHKDEL              NO - DON'T NEED THIS LINE                    
*                                                                               
         GOTO1 VLUPPROG                                                         
         GOTO1 VLUPHUT                                                          
         BAS   RE,VALOVER                                                       
         OC    SHARE,SHARE         COMPUTE RATING                               
         BZ    VALL7                                                            
         TM    NPUBOVRD,X'20'      UNLESS IT HAS BEEN OVERRIDDEN                
         BO    VALL7                                                            
         SR    R0,R0                                                            
         ICM   R0,3,HUT                                                         
         SR    R1,R1                                                            
         ICM   R1,3,SHARE                                                       
         MR    R0,R0                                                            
         D     R0,=F'500'                                                       
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STCM  R1,3,RATING                                                      
         SPACE 1                                                                
VALL7    MVC   NPUBSHR,SHARE                                                    
         MVC   NPUBHUT,HUT                                                      
         MVC   NPUBRTG,RATING                                                   
         MVC   NPUBBOOK,PBEL+5     CARRY BOOK YEAR/WEEK                         
         BAS   RE,SETVPH           CHECK VPH ELEMENT IS IN RECORD               
         MVC   NPUBLNK,VPHLINK     AND LINK TO IT                               
         XC    NPUBVOVR,NPUBVOVR                                                
*                                                                               
         CLI   PLANPRCB,C'Y'       CABLE PROGRAM                                
         BNE   *+24                NO,REGULAR PROCESSING                        
         OC    CABDEMS,CABDEMS     CHECK FOR DEMOS                              
         BZ    *+14                                                             
         OI    NPUBOVRD,X'10'      SET VPH OVERRIDES                            
         MVC   NPUBVOVR(8),CABDEMS MOVE DEMOS OUT                               
*                                                                               
         OC    OVERAMNT,OVERAMNT                                                
         BZ    *+10                                                             
         MVC   NPUBVOVR(2),OVERAMNT                                             
         MVI   ELCODE,X'12'        NOW SEE IF ELEMENT THERE ALREADY             
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     VALL10                                                           
         SPACE 1                                                                
VALL8    BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
VALL10   BNE   VALLNO                                                           
         CLC   ELEMENT+2(2),2(R6)  CHECK FOR SAME PERIOD                        
         BNE   VALL8                                                            
         MVC   2(16,R6),ELEMENT+2  ELEMENT IS ALREADY THERE, SO                 
         MVC   36(2,R6),ELEMENT+36 OVERWRITE UP TO FIRST OVERRIDE               
         BAS   RE,CHKCOST                                                       
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
*              REMOVE COST FOR LENGTHS WITH ZERO                                
         SPACE 3                                                                
*              INPUTS              R6 = ADDRESS OF ELEMENT                      
         SPACE 1                                                                
CHKCOST  NTR1                                                                   
         USING NPUBD,R6                                                         
         LA    RE,NPUBUNS                                                       
         LA    RF,NPUBAMT          GET L'VPHS IN R3                             
         LA    R1,4                                                             
*                                                                               
CHKCST20 CLI   0(RE),0                                                          
         BNE   CHKCST40                                                         
         XC    0(4,RF),0(RF)                                                    
CHKCST40 LA    RE,1(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,CHKCST20                                                      
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              MAINTAIN THE VPH ELEMENTS                                        
         SPACE 3                                                                
*              INPUTS              VPHS CONTAINS VPHS!                          
*              OUTPUTS             LINK NUMBER RETURNED IN VPHLINK              
         SPACE 1                                                                
SETVPH   NTR1                                                                   
         LA    R1,VPHS+239                                                      
         LA    R3,240              GET L'VPHS IN R3                             
         SPACE 1                                                                
VPHLEN   CLI   0(R1),0                                                          
         BNE   VPHLEN2                                                          
         BCTR  R1,0                                                             
         BCT   R3,VPHLEN                                                        
         LA    R3,2                                                             
         SPACE 1                                                                
VPHLEN2  BCTR  R3,0                                                             
****     ZIC   R3,VPHSLEN          PXZ 11/1 PASSED IN NEPUP00N                  
         MVI   ELCODE,X'14'                                                     
         USING NPUCD,R6                                                         
         L     R6,AIO                                                           
         SR    R2,R2               COUNT HOW MANY VPHS ELEMENTS SO FAR          
         BAS   RE,GETEL                                                         
         B     SETVPH4                                                          
         SPACE 1                                                                
SETVPH2  BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
SETVPH4  BNE   SETVPH6                                                          
         MVC   VPHLINK,NPUCLNK     RETURN LINK IF MATCH                         
         LA    R2,1(R2)                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   VPHS(0),NPUCVPHS    CHECK FOR MATCHING VPH                       
         BNE   SETVPH2                                                          
*  VPHS MATCH/ NOW CHECK NADS MATCH                                             
         LR    R1,R6               SAVE CURRENT R6 POINTER                      
         MVI   ELCODE,X'16'       .ANY NADS                                     
SETVPH4B BAS   RE,NEXTEL                                                        
         BE    SETVPH5                                                          
         CLI   NOVERS,0           .NO/ANY IN NOVERS                             
         BNE   SETVPH5A               YES/NO MATCH                              
         B     XIT                    NO/OK/XIT                                 
SETVPH5  CLC   VPHLINK,2(R6)      CHECK LINK TO NADS                            
         BNE   SETVPH4B                                                         
         ZIC   R4,1(R6)                                                         
         S     R4,=F'5'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   NOVERS(0),4(R6)                                                  
         BE    XIT                 NADS MATCH /OK/XIT                           
SETVPH5A LR    R6,R1          NADS DON'T MATCH/RESET REC ELEM POINTER           
         MVI   ELCODE,X'14'                    RESET ELCODE                     
         B     SETVPH2        GET NEXT VPH ELEM                                 
         SPACE 1                                                                
SETVPH6  LA    R2,1(R2)                                                         
         CH    R2,=H'6'            ONLY ROOM FOR 6 VPH ELEMENTS                 
         BH    XIT                 (VPHLINK IS SET TO LAST)                     
         MVC   WORK(40),ELEMENT    SAVE THE X'12' ELEMENT                       
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         MVI   NPUCEL,X'14'        ADD ANOTHER VPH ELEMENT                      
         STC   R2,NPUCLNK                                                       
         STC   R2,VPHLINK                                                       
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   NPUCVPHS(0),VPHS                                                 
         LA    R3,1+4(R3)                                                       
         STC   R3,NPUCLEN                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
* ANY NADS TO ADD                                                               
         CLI   NOVERS,0                                                         
         BE    SETVPH10                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,X'16'                                                    
         MVC   ELEMENT+2(1),VPHLINK                                             
         SR    R1,R1                                                            
         LA    R4,NOVERS                                                        
SETVPH8  CLI   0(R4),0                                                          
         BE    SETVPH9                                                          
         LA    R4,5(R4)            BUMP TO NXT NAD SET                          
         LA    R1,5(R1)                                                         
         B     SETVPH8                                                          
SETVPH9  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+4(0),NOVERS                                              
         LA    R1,5(R1)                                                         
         STC   R1,ELEMENT+1                                                     
         GOTO1 ADDELEM                                                          
*                                                                               
SETVPH10 XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(40),WORK                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OVERRIDES                                    
         SPACE 3                                                                
VALOVER  NTR1                                                                   
         USING NPUBD,R6                                                         
         XC    OVEREL,OVEREL                                                    
         CLI   5(R2),0             ANY INPUT                                    
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK)                                      
         MVI   FIELDERR,1                                                       
         LA    R3,BLOCK                                                         
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    BADOVER                                                          
         SPACE 1                                                                
VALOV2   LA    R5,SHARE            SHARE                                        
         MVI   ACTUAL,X'80'                                                     
         CLI   12(R3),C'S'                                                      
         BE    VALOV4                                                           
         LA    R5,HUT              HUT                                          
         MVI   ACTUAL,X'40'                                                     
         CLI   12(R3),C'H'                                                      
         BE    VALOV4                                                           
         LA    R5,RATING           RATING                                       
         MVI   ACTUAL,X'20'                                                     
         CLI   12(R3),C'R'                                                      
         BE    VALOV4                                                           
         LA    R5,OVERAMNT         VPH FOR TARGET DEMOS                         
         MVI   ACTUAL,X'10'                                                     
         CLI   12(R3),C'V'                                                      
         BE    VALOV4                                                           
         B     BADOVER                                                          
         SPACE 1                                                                
VALOV4   OC    NPUBOVRD,ACTUAL                                                  
         L     R1,8(R3)            VPH IS STRAIGHT NUMERIC                      
         CLI   12(R3),C'V'                                                      
         BE    VALOV6                                                           
         ZIC   R1,1(R3)            PICK UP L'DECIMAL FIELD                      
         LTR   R1,R1                                                            
         BZ    BADOVER                                                          
         ST    R1,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(1,22(R3))                                          
         L     R1,DMCB+4                                                        
         CH    R1,=H'999'                                                       
         BH    BADOVER                                                          
         SPACE 1                                                                
VALOV6   LTR   R1,R1                                                            
         BZ    BADOVER                                                          
         STH   R1,0(R5)                                                         
         SPACE 1                                                                
VALOVNXT LA    R3,32(R3)                                                        
         BCT   R4,VALOV2                                                        
         B     XIT                                                              
         EJECT                                                                  
*              WRITE THE RECORD OUT (FOR STADD ACTION ONLY)                     
         SPACE 3                                                                
*              INPUTS              AIO POINTS TO RECORD                         
         SPACE 1                                                                
UPDREC   NTR1                                                                   
         LA    R2,PUPCLIH                                                       
*                                                                               
         L     R6,AIO                                                           
         MVC   KEY(20),0(R6)                                                    
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(20),KEYSAVE                                                  
         BE    UPDR100                                                          
         GOTO1 ADDREC                                                           
         B     UPDREX                                                           
*                                                                               
*--RECORD EXISTS CHECK FOR DELETE IF SO UPDATE RECORD AND WRITE BACK            
*                                                                               
UPDR100  TM    KEY+20,X'80'        IF DELETED                                   
         BZ    BADADD              NO, ERROR RECORD ALREADY EXISTS              
*                                  THEN UNDELETE IT AND WRITE RECORD            
         NI    KEY+20,X'FF'-X'80'  WRITE THE KEY BACK                           
         GOTO1 WRITE                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC              GET OLD RECORD                               
         MVC   AIO,AIO1            REPLACE WITH NEW RECORD                      
         GOTO1 PUTREC                                                           
*                                                                               
UPDREX   B     XIT                                                              
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
         BAS   RE,SHOWPLAN                                                      
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
         CLI   PLANPRCB,C'Y'                                                    
         BNE   DREC05                                                           
         MVC   PROGCODE,PUPPROG                                                 
         GOTO1 VLUPCPRG            GET CABLE PROGARM INFO                       
         B     DREC10                                                           
DREC05   GOTO1 VEXTPROG                                                         
DREC10   BAS   RE,SHOWPROG                                                      
         XC    BUFF(64),BUFF                                                    
         LA    R5,BUFF                                                          
         USING BUFFD,R5                                                         
         LA    R2,PUPUNITH                                                      
         CLI   PLANPERT,C'W'       IF WE ARE DOING WEEKS                        
         BNE   DREC30                                                           
         LA    R3,OMONDAYS         DEAL WITH 'OTHER' WEEKS FIRST                
         LA    R4,3                                                             
         SPACE 1                                                                
DREC20   BAS   RE,DLINE            THIS IS SO OTHERS ADD INTO TOTALS            
         LA    R3,2(R3)                                                         
         BCT   R4,DREC20                                                        
         MVI   HIDDEN,C'N'         TAKE A NOTE IF THERE WERE ANY                
         OC    TOTUNS(16),TOTUNS                                                
         BZ    DREC30                                                           
         MVI   HIDDEN,C'Y'                                                      
         SPACE 1                                                                
DREC30   LA    R3,QPERLIST                                                      
         LA    R4,4                                                             
         CLI   PLANPERT,C'Q'                                                    
         BE    DREC60                                                           
         LA    R3,MPERLIST                                                      
         LA    R4,13                                                            
         CLI   PLANPERT,C'M'                                                    
         BE    DREC60                                                           
         LA    R3,CMONDAYS                                                      
         SPACE 1                                                                
DREC60   BAS   RE,DLINE                                                         
         LA    R0,7                                                             
         BAS   RE,BUMPN                                                         
         LA    R3,2(R3)                                                         
         BCT   R4,DREC60                                                        
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
         BAS   RE,DOVER                                                         
         BAS   RE,BUMP                                                          
         BAS   RE,DDEMOS                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY OVERRIDES                                     
         SPACE 3                                                                
DOVER    NTR1                                                                   
         MVC   8(12,R2),SPACES                                                  
         OI    6(R2),X'80'                                                      
         CLI   NPUBOVRD,0                                                       
         BE    XIT                                                              
         LA    R3,BLOCK                                                         
         MVC   BLOCK(20),SPACES                                                 
         SPACE 1                                                                
         TM    NPUBOVRD,X'80'      SHARE                                        
         BNO   DOVER2                                                           
         MVC   0(2,R3),=C'S='                                                   
         LA    R3,2(R3)                                                         
         EDIT  (2,NPUBSHR),(5,0(R3)),1,ALIGN=LEFT                               
         AR    R3,R0                                                            
         SH    R3,=H'2'                                                         
         CLC   0(2,R3),=C'.0'      DROP REDUNDANT .0                            
         BNE   *+14                                                             
         MVC   0(2,R3),SPACES                                                   
         B     *+8                                                              
         LA    R3,2(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
DOVER2   TM    NPUBOVRD,X'40'      HUT                                          
         BNO   DOVER4                                                           
         MVC   0(2,R3),=C'H='                                                   
         LA    R3,2(R3)                                                         
         EDIT  (2,NPUBHUT),(5,0(R3)),1,ALIGN=LEFT                               
         AR    R3,R0                                                            
         SH    R3,=H'2'                                                         
         CLC   0(2,R3),=C'.0'      DROP REDUNDANT .0                            
         BNE   *+14                                                             
         MVC   0(2,R3),SPACES                                                   
         B     *+8                                                              
         LA    R3,2(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
DOVER4   TM    NPUBOVRD,X'20'      RATING                                       
         BNO   DOVER6                                                           
         MVC   0(2,R3),=C'R='                                                   
         LA    R3,2(R3)                                                         
         EDIT  (2,NPUBRTG),(5,0(R3)),1,ALIGN=LEFT                               
         AR    R3,R0                                                            
         SH    R3,=H'2'                                                         
         CLC   0(2,R3),=C'.0'      DROP REDUNDANT .0                            
         BNE   *+14                                                             
         MVC   0(2,R3),SPACES                                                   
         B     *+8                                                              
         LA    R3,2(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
DOVER6   TM    NPUBOVRD,X'10'      VPH                                          
         BNO   DOVER8                                                           
         MVC   0(2,R3),=C'V='                                                   
         LA    R3,2(R3)                                                         
         LA    R4,NPUBVOVR                                                      
         CLI   0(R6),X'12'                                                      
         BE    *+8                                                              
         USING NPUAD,R6                                                         
         LA    R4,NPUAVOVR         OLD EL HAS OVERRIDE ELSEWHERE                
         USING NPUBD,R6                                                         
         EDIT  (2,0(R4)),(5,0(R3)),ALIGN=LEFT                                   
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
DOVER8   BCTR  R3,0                                                             
         MVI   0(R3),C' '                                                       
         MVC   8(12,R2),BLOCK                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY DEMOS                                         
         SPACE 3                                                                
*              INPUTS              R2=A(FIELD HEADER)                           
*                                  R4=TOTAL UNITS                               
*                                  R5=A(BUFFD - TOTRTG HOMES IMPS)              
         SPACE 1                                                                
DDEMOS   NTR1                                                                   
         MVC   8(40,R2),SPACES                                                  
         OI    6(R2),X'80'                                                      
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         SPACE 1                                                                
         LA    R2,8(R2)                                                         
         EDIT  (2,NPUBSHR),(4,0(R2)),1,ZERO=BLANK                               
         OC    NPUBSHR,NPUBSHR     DEDUCE SHARE IF MISSING                      
         BNZ   DDEMOS1                                                          
         LH    R1,NPUBRTG                                                       
         M     R0,=F'2000'                                                      
         LH    RF,NPUBHUT                                                       
         LTR   RF,RF                                                            
         BZ    DDEMOS1                                                          
         DR    R0,RF                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(4,0(R2)),1,ZERO=BLANK                                      
         SPACE 1                                                                
DDEMOS1  LA    R2,5(R2)                                                         
         EDIT  (2,NPUBHUT),(4,0(R2)),1,ZERO=BLANK                               
         LA    R2,5(R2)                                                         
         EDIT  (2,NPUBRTG),(4,0(R2)),1,ZERO=BLANK                               
         LA    R2,5(R2)                                                         
         MVI   LENGTH,0                                                         
         MVC   GDDEMO,=X'000001'                                                
*        MVI   GDDEMO,1                                                         
         GOTO1 VGETDEM                                                          
         L     R1,GDTGRP           HOME GRPS                                    
         A     R1,TOTGRPS                                                       
         ST    R1,TOTGRPS                                                       
         L     R1,GDTGRP                                                        
         EDIT  (R1),(5,0(R2)),1,ZERO=BLANK                                      
         CH    R1,=H'10000'                                                     
         BL    DDEMOS2                                                          
         AH    R1,=H'5'                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         EDIT  (R1),(5,0(R2))                                                   
         SPACE 1                                                                
DDEMOS2  LA    R2,6(R2)            HOMES                                        
         EDIT  (4,GDTIMP),(7,0(R2))                                             
         L     R1,GDTIMP                                                        
         A     R1,TOTHOMES                                                      
         ST    R1,TOTHOMES                                                      
         SPACE 1                                                                
         MVC   GDDEMO,TARGET                                                    
***      MVC   GDDEMO,TARGET+2                                                  
         GOTO1 VGETDEM                                                          
         LA    R2,8(R2)                                                         
         L     R1,GDVPH                                                         
         EDIT  (R1),(4,0(R2))                                                   
         SPACE 1                                                                
DDEMOS4  LA    R2,4(R2)            DEMO IMPS                                    
         EDIT  (4,GDTIMP),(7,0(R2))                                             
         L     R1,GDTIMP                                                        
         A     R1,TOTIMPS                                                       
         ST    R1,TOTIMPS                                                       
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
         MVC   8(12,R2),SPACES                                                  
         OI    6(R2),X'80'                                                      
         CLI   HIDDEN,C'Y'         IF THERE ARE HIDDEN UNITS                    
         BNE   *+10                   GIVE A LITTLE MESSAGE!                    
         MVC   8(11,R2),=C'SOME HIDDEN'                                         
         BAS   RE,BUMP                                                          
         MVC   8(40,R2),SPACES                                                  
         OI    6(R2),X'80'                                                      
         LA    R2,22(R2)                                                        
         EDIT  (4,TOTGRPS),(6,0(R2)),1,ZERO=BLANK                               
         LA    R2,7(R2)                                                         
         EDIT  (4,TOTHOMES),(7,0(R2))                                           
         LA    R2,12(R2)                                                        
         EDIT  (4,TOTIMPS),(7,0(R2))                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MODIFY TWA FOR PLAN                                   
         SPACE 3                                                                
MODTWA   NTR1                                                                   
         MVI   MODSW,C'N'                                                       
         TM    PUPOVERH+1,X'20'                                                 
         BNO   MODTWA2                                                          
         MVI   MODSW,C'Y'                                                       
         SPACE 1                                                                
MODTWA2  LA    R2,PUPLENS          SHOW LENGTHS IN HEADINGS                     
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
         LA    R2,PUPPERH                                                       
         LA    R3,QLIST                                                         
         LA    R4,14                                                            
         CLI   PLANPERT,C'Q'                                                    
         BE    MODTWA6                                                          
         LA    R3,MLIST                                                         
         CLI   PLANPERT,C'M'                                                    
         BE    MODTWA6                                                          
         BAS   RE,WEEKDATS                                                      
         LA    R3,OVWORK+15                                                     
         SPACE 1                                                                
MODTWA6  BAS   RE,MODALINE                                                      
         LA    R0,7                                                             
         BAS   RE,BUMPN            (SKIP PAST 7 FIELDS)                         
         LA    R3,5(R3)                                                         
         BCT   R4,MODTWA6                                                       
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
         CLC   =C'MAR26',8(R2)     FUDGE: IF MAR26                              
         BNE   SKPFDG                                                           
         CLI   PROGPERQ,1          AND 1ST QUARTER                              
         BNE   SKPFDG                                                           
         CLI   N0PROF+3,C'B'                                                    
         BNE   SKPFDG                                                           
         XC    8(5,R2),8(R2)       CLEAR DATE                                   
*                                                                               
SKPFDG   OI    6(R2),X'80'                                                      
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
         CLI   MODSW,C'Y'                                                       
         BE    MODL3                                                            
         CLC   PLANNLEN,PUPHIDE+10 MODIFY FIELDS IF N'LENGTHS                   
         BNE   MODL3                                                            
         CLC   LASTQURT(1),PUPQURT OR QUARTER                                   
         BNE   MODL3                                                            
         CLC   PLANPERT,PUPHIDE+12 OR PERIOD TYPE HAVE CHANGED                  
         BE    XIT                                                              
         SPACE 1                                                                
MODL3    MH    R4,=H'6'                                                         
         LA    R4,PROTLIST(R4)                                                  
         LA    R0,6                                                             
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
PROTLIST DC    C'YYYYYY'                                                        
         DC    C'NYYYNY'                                                        
         DC    C'NNYYNY'                                                        
         DC    C'NNNYNY'                                                        
         DC    C'NNNNNY'                                                        
         EJECT                                                                  
*              ROUTINE TO SHOW PLAN DETAILS ON SCREEN                           
         SPACE 3                                                                
SHOWPLAN NTR1                                                                   
         XC    PUPTAR2,PUPTAR2     CLEAR DEMO DISPLAY LINES                     
         OI    PUPTAR2H+6,X'80'                                                 
         XC    PUPTARG,PUPTARG                                                  
         OI    PUPTARGH+6,X'80'                                                 
         CLI   TARGET,0                                                         
         BE    SHOWPL4                                                          
         EDIT  (1,TARGET),(3,PUPTAR2),FILL=0,ZERO=NOBLANK                       
         OI    PUPTAR2H+6,X'80'                                                 
         LA    RE,4                                                             
         LA    RF,TARGNAME                                                      
SHOWPL2  CLI   0(RF),C'.'                                                       
         BE    *+14                                                             
         LA    RF,1(RF)                                                         
         BCT   RE,SHOWPL2                                                       
         DC    H'0'                 MUST HAVE NAD SETUP                         
         MVC   PUPTARG,0(RF)                                                    
         B     *+10                                                             
*        CLI   TARGNAME+2,C'.'     NAD DEMO                                     
*        BNE   SHOWPL4                                                          
*        MVC   PUPTAR2,TARGNAME   YES/BREAK IT UP                               
*        MVC   PUPTARG,TARGNAME+3                                               
*        OI    PUPTAR2H+6,X'80'                                                 
*        B     *+10                                                             
SHOWPL4  MVC   PUPTARG,TARGNAME                                                 
         OI    PUPTARGH+6,X'80'                                                 
         LA    R2,PUPHUTS                                                       
         EDIT  (1,PLANHTYR),(2,0(R2)),FILL=0,ZERO=NOBLANK                       
         CLI   PLANHTNO,2                                                       
         BL    SHOWPL6                                                          
         MVI   2(R2),C','                                                       
         EDIT  (1,PLANHTNO),(1,3(R2))                                           
         SPACE 1                                                                
SHOWPL6  MVC   5(1,R2),PLANHTAV    HUT AVERAGE                                  
         MVC   6(1,R2),PLANHTFL        MONTH FLAVOR                             
         MVC   7(1,R2),PLANHTSC        SCHEME                                   
         OI    PUPHUTSH+6,X'80'                                                 
         B     XIT                                                              
         SPACE 3                                                                
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
*        GOTO1 DATCON,DMCB,(0,WORK),(0,WORK)   Y2K                              
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
*                                                                               
         CLC   WORK(4),=C'0010'    ,,IF 3D QRT 00                               
         BNE   WKD1B                                                            
*        CLC   WORK(4),=C'9610'    ,,IF 3D QRT 96                               
*        BNE   WKD1B                                                            
         CLI   N0PROF+3,C'C'       ,,IF CALENDAR                                
         BNE   WKD1B                                                            
         A     R1,=F'-7'           ,,DROP ONE WEEK                              
         ST    R1,DMCB+8                                                        
*                                                                               
WKD1B    CLC   WORK(4),=C'0103'    ,,IF 1S QRT 01                               
         BNE   WKD1C                                                            
         CLI   N0PROF+3,C'B'       ,,IF BROADCAST                               
         BNE   WKD1C                                                            
         A     R1,=F'7'            ,,ADD ONE WEEK                               
         ST    R1,DMCB+8                                                        
*                                                                               
WKD1C    CLC   WORK(4),=C'0110'    ,,IF 1S QRT 01                               
         BNE   WKD1D                                                            
         CLI   N0PROF+3,C'C'       ,,IF CALENDAR                                
         BNE   WKD1D                                                            
         A     R1,=F'-7'            ,,DROP ONE WEEK                             
         ST    R1,DMCB+8                                                        
*                                                                               
WKD1D    CLC   WORK(6),=C'020331'  ,,IF 1S QRT 02                               
         BNE   WKD1E                                                            
         CLI   N0PROF+3,C'C'       ,,IF CALANDER                                
         BNE   WKD1E                                                            
         A     R1,=F'7'            ,,ADD ONE WEEK                               
         ST    R1,DMCB+8                                                        
*                                                                               
WKD1E    CLC   WORK(6),=C'020630'  ,,IF 2N QRT 02                               
         BNE   WKD2B                                                            
         CLI   N0PROF+3,C'C'       ,,IF CALANDER                                
         BNE   WKD2B                                                            
         A     R1,=F'7'            ,,ADD ONE WEEK                               
         ST    R1,DMCB+8                                                        
*                                                                               
WKD2B    GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         MVC   WORK(6),WORK+6                                                   
         SPACE 1                                                                
*-- NEXT FOUR LINES ARE NEEDED TO CORRECT A FLAW IN THE 1995                    
*-- YEAR DATE CALCULATIONS, DEALING WITH THE THIRD QUARTER.                     
WKD2     CLC   WORK(6),=CL6'001001'                                             
         BNE   *+12                                                             
*        CLC   WORK(6),=CL6'951001'                                             
*        BNE   *+12                                                             
         CLI   N0PROF+3,C'B'       IF WE ARE USING BROADCAST                    
         BE    WKD1                   ADD IN ANOTHER WEEK                       
*-- NEXT FOUR LINES ARE NEEDED TO CORRECT A FLAW IN THE 2002                    
*-- YEAR DATE CALCULATIONS, DEALING WITH FIRST AND SECOND QTR                   
         CLC   WORK(6),=CL6'020331'                                             
         BE    *+14                                                             
         CLC   WORK(6),=CL6'020630'                                             
         BNE   *+12                                                             
         CLI   N0PROF+3,C'C'       IF WE ARE USING CALANDER                     
         BE    WKD1                   ADJUST                                    
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
BADOVER  MVC   CONHEAD(L'OVERERR),OVERERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADUNIT  MVC   CONHEAD(L'UNITERR),UNITERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADQUART MVC   CONHEAD(L'QURTERR),QURTERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADADD   MVC   CONHEAD(L'ADDERR),ADDERR                                         
         B     MYEND                                                            
         SPACE 1                                                                
OVERERR  DC    C'** ERROR ** INVALID OVERRIDE EXPRESSION'                       
UNITERR  DC    C'** ERROR ** NEED NUMBER OF UNITS'                              
ADDERR   DC    C'** ERROR ** RECORD ALREADY EXISTS CANNOT ADD'                  
QURTERR  DC    C'NEED QUARTER NUMBER (1-4) FOR WEEKLIES'                        
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPN    ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R0,BUMPN                                                         
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
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR MODULE STORAGE                                         
         SPACE 3                                                                
BUFFD    DSECT                                                                  
TOTUNS   DS    4F                  UNITS FOR LENGTHS 1-4                        
TOTGRPS  DS    F                                                                
TOTHOMES DS    F                                                                
TOTIMPS  DS    F                                                                
         DS    CL36                SPARE                                        
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPE1D                                                       
         SPACE 1                                                                
         ORG   CONTAGH+2600                                                     
PUPHIDE  DS    CL16                                                             
MODSW    DS    CL1                                                              
SAVPRKEY DS    CL32                                                             
CMONDAYS DS    CL26                MONDAYS ON THE SCREEN                        
OMONDAYS DS    CL6                 HIDDEN MONDAYS                               
BACKOPT  DS    CL1                                                              
VPHLINK  DS    XL1                                                              
LASTQURT DS    XL2                                                              
HIDDEN   DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016NEPUP11   05/01/02'                                      
         END                                                                    

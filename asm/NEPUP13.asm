*          DATA SET NEPUP13    AT LEVEL 012 AS OF 05/01/02                      
*          DATA SET NEPUP13    AT LEVEL 065 AS OF 08/10/90                      
*PHASE T32213A,*                                                                
         TITLE 'T32213 - PROGRAM OVERRIDES'                                     
T32213   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32213**,RA                                                    
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
         BE    *+10                                                             
         XC    LASTPKEY,LASTPKEY                                                
         CLC   TWALREC,RECNUM                                                   
         BE    *+10                                                             
         XC    LASTPKEY,LASTPKEY                                                
         SPACE 1                                                                
MODE1    CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         GOTO1 READ                                                             
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         CLC   LASTPKEY,KEY                                                     
         BE    MODE4                                                            
         SPACE 1                                                                
MODE3    MVC   LASTPKEY,KEY                                                     
         GOTO1 GETREC                                                           
         BAS   RE,DREC             KEYS ARE DIFFERENT SO DISPLAY                
         MVC   CONHEAD,=CL60'DATA DISPLAYED - ENTER OVERRIDES'                  
         OI    CONHEADH+6,X'80'                                                 
         LA    R2,PUPOVERH                                                      
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
         SPACE 1                                                                
VKEY2    BAS   RE,SHOWPLAN                                                      
         SPACE 1                                                                
         LA    R2,PUPPROGH         PROGRAM                                      
         GOTO1 ANY                                                              
         MVC   PROGCODE,WORK                                                    
         ZIC   R1,PLANYEAR         USE START YEAR                               
         BCTR  R1,0                                                             
         STC   R1,PERIOD                                                        
         MVI   PERIOD+1,3          SET PERIOD FOR 3RD QUARTER                   
         CLI   PLANPERT,C'Q'                                                    
         BE    VKEY4                                                            
         MVI   PERIOD+1,9          OR SEPTEMBER IF MONTHLY                      
         CLI   PLANPERT,C'M'                                                    
         BE    VKEY4                                                            
         ZIC   R1,PERIOD                                                        
         CLI   PROGPERQ,0          SELECTED DATE FOR WEEKLIES                   
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,WORK                                                          
         ZIC   R1,PROGPERQ                                                      
         SLL   R1,1                                                             
         LA    R1,FWEEKS(R1)                                                    
         MVC   WORK+1(2),0(R1)                                                  
         GOTO1 DATCON,DMCB,(3,WORK),(2,PERIOD)                                  
         SPACE 1                                                                
VKEY4    GOTO1 VLUPPROG                                                         
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
         SPACE 1                                                                
FWEEKS   DC    AL1(9,15,1,1,4,1,7,1)                                            
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         LA    R2,PUPOVERH                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         B     VREC4                                                            
         SPACE 1                                                                
VREC2    MVI   ELCODE,X'12'                                                     
         BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
VREC4    BNE   XIT                                                              
         BAS   RE,VALALINE                                                      
         LA    R0,10                                                            
         BAS   RE,BUMPN                                                         
         B     VREC2                                                            
         EJECT                                                                  
*              VALIDATE A LINE OF INPUT                                         
         SPACE 3                                                                
*              INPUTS              R2=A(FIRST INPUT)                            
*                                  R6=A(NEW UNIT ASSIGNMENT ELEMENT)            
         SPACE 1                                                                
VALALINE NTR1                                                                   
         USING NPUBD,R6                                                         
         MVC   PERIOD,NPUBPER                                                   
         GOTO1 VLUPPROG            RELOOK PROGRAM                               
         GOTO1 VLUPHUT                    AND HUTS                              
         SPACE 1                                                                
VALL2    MVI   ACTUAL,1                                                         
         NI    NPUBOVRD,X'20'      (DON'T TURN OFF RATING BIT)                  
         SPACE 1                                                                
         LA    R4,SHARE            TEST SHARE                                   
         BAS   RE,VALOVER                                                       
         BE    *+8                                                              
         OI    NPUBOVRD,X'80'                                                   
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
         LA    R4,HUT              TEST HUT                                     
         BAS   RE,VALOVER                                                       
         BE    *+8                                                              
         OI    NPUBOVRD,X'40'                                                   
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
         LA    R4,RATING           TEST RATING                                  
         BAS   RE,VALOVER                                                       
         BE    *+8                                                              
         OI    NPUBOVRD,X'20'                                                   
         SPACE 1                                                                
         BAS   RE,BUMP                                                          
         OC    SHARE,SHARE         COMPUTE RATING                               
         BZ    VALL4                                                            
         TM    NPUBOVRD,X'20'              UNLESS OVERRIDDEN                    
         BO    VALL4                                                            
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
VALL4    MVC   NPUBSHR,SHARE                                                    
         MVC   NPUBHUT,HUT                                                      
         MVC   NPUBRTG,RATING                                                   
         SPACE 1                                                                
         XC    NPUBVOVR,NPUBVOVR                                                
         LA    R3,DEMOS                                                         
         LA    R4,NPUBVOVR                                                      
         ZIC   R0,NDEMOS                                                        
         MVI   ACTUAL,0            (NO DECIMALS FOR VPH)                        
         SPACE 1                                                                
VALL6    MVC   GDDEMO,0(R3)        LOOK UP THE VPH                              
*VALL6    MVC   GDDEMO,2(R3)        LOOK UP THE VPH                             
         MVI   LENGTH,0                                                         
         GOTO1 VGETDEM                                                          
         MVC   0(2,R4),GDVPH+2                                                  
         BAS   RE,VALOVER                                                       
         BE    VALL8                                                            
         ZIC   R1,NDEMOS                                                        
         CR    R0,R1               IF FIRST VPH IS OVERRIDDEN                   
         BNE   VALL20                                                           
         OI    NPUBOVRD,X'10'         MAKE A NOTE                               
         B     VALL20                                                           
         SPACE 1                                                                
VALL8    XC    0(2,R4),0(R4)       NOV OVERRIDE - SO CREAM                      
         SPACE 1                                                                
VALL20   BAS   RE,BUMP                                                          
         LA    R3,3(R3)                                                         
         LA    R4,2(R4)                                                         
         BCT   R0,VALL6                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE OVERRIDE                                                
         SPACE 3                                                                
*              INPUT               R2=A(HEADER)                                 
*                                  R4=A(HALF WORD OLD VALUE)                    
*                                  ACTUAL 1=1 DECIMAL ALLOWED                   
*              OUTPUT              REPLACE HALF WORD WITH NEW VALUE             
*                                  SET CONDITION CODE. SAME IS =                
         SPACE 1                                                                
VALOVER  NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    NOOVER              (NO DATA = NO OVERRIDE)                      
         TM    4(R2),X'20'         ANY INPUT IN THIS FIELD?                     
         BO    NOOVER                                                           
         LA    R0,6                                                             
         LA    R1,13(R2)                                                        
         SPACE 1                                                                
VALOVER2 CLI   0(R1),C'0'          GET L'FIELD INTO R0                          
         BNL   VALOVER4                                                         
         BCTR  R1,0                                                             
         BCT   R0,VALOVER2                                                      
         LTR   R0,R0                                                            
         B     NOOVER              (NO DATA = NO OVERRIDE)                      
         SPACE 1                                                                
VALOVER4 ST    R0,DMCB+4           (LENGTH OF EXPRESSION)                       
         GOTO1 CASHVAL,DMCB,(ACTUAL,8(R2))                                      
         CLI   DMCB,X'FF'                                                       
         BE    BADOVER                                                          
         L     R1,DMCB+4                                                        
         SR    R0,R0                                                            
         CLI   ACTUAL,1                                                         
         BE    *+8                                                              
         D     R0,=F'100'                                                       
         CH    R1,=H'9999'                                                      
         BH    BADOVER                                                          
         STH   R1,0(R4)                                                         
         SPACE 1                                                                
YESOVER  LA    R0,1                OVERRIDE NOTICED                             
         B     *+6                                                              
         SPACE 1                                                                
NOOVER   SR    R0,R0               NO OVERRIDE                                  
         LTR   R0,R0               (SET THE CONDITION CODE)                     
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 3                                                                
DREC     NTR1                                                                   
         SPACE 1                                                                
         GOTO1 VEXTPROG                                                         
         BAS   RE,SHOWPROG                                                      
         LA    R2,PUPPERH                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'12'                                                     
         LA    R4,16                                                            
         BAS   RE,GETEL                                                         
         B     DREC4                                                            
         SPACE 1                                                                
DREC2    MVI   ELCODE,X'12'                                                     
         BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
DREC4    BNE   DREC6                                                            
         BAS   RE,DPER             FOR EACH ELEMENT DISPLAY PERIOD              
         BAS   RE,BUMP                                                          
         BAS   RE,DLINE            AND PRESENT VALUES                           
         LA    R0,9                                                             
         BAS   RE,BUMPN                                                         
         BCT   R4,DREC2                                                         
         B     XIT                                                              
         SPACE 1                                                                
DREC6    MH    R4,=H'10'           CLEAR REST OF SCREEN                         
         SPACE 1                                                                
DREC8    MVC   8(6,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         BCT   R4,DREC8                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS A PERIOD                                        
         SPACE 3                                                                
*              INPUTS              R2=A(HEADER)                                 
*                                  R6=A(X'12' ELEMENT)                          
         SPACE 1                                                                
DPER     NTR1                                                                   
         USING NPUBD,R6                                                         
         OI    6(R2),X'80'                                                      
         LA    R2,8(R2)            ADDRESS FIELD                                
         MVC   0(6,R2),SPACES                                                   
         CLI   PLANPERT,C'Q'                                                    
         BE    DPERQ                                                            
         CLI   PLANPERT,C'M'                                                    
         BE    DPERM                                                            
         B     DPERW                                                            
         SPACE 1                                                                
DPERQ    MVI   0(R2),C'Q'          SHOW QUARTER                                 
         MVC   1(1,R2),NPUBPER+1   QUARTER NUMBER                               
         OI    1(R2),X'F0'                                                      
         CLI   1(R2),C'0'                                                       
         BNE   *+8                                                              
         MVI   1(R2),C'4'                                                       
         MVI   2(R2),C'/'                                                       
         EDIT  (1,NPUBPER),(2,3(R2))                                            
         B     XIT                                                              
         SPACE 1                                                                
DPERM    ZIC   R1,NPUBPER+1        SHOW MONTH                                   
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MYMONTHS(R1)                                                  
         MVC   0(3,R2),0(R1)                                                    
         MVI   3(R2),C'/'                                                       
         EDIT  (1,NPUBPER),(2,4(R2))                                            
         B     XIT                                                              
         SPACE 1                                                                
*                                  SHOW WEEK DATE                               
DPERW    GOTO1 DATCON,DMCB,(2,NPUBPER),(4,0(R2))                                
         B     XIT                                                              
         SPACE 1                                                                
MYMONTHS DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
         EJECT                                                                  
*              ROUTINE DISPLAYS A LINE OF FIELDS                                
         SPACE 3                                                                
*              INPUTS              R2=A(FIRST OVERRIDE HEADER)                  
*                                  R6=A(X'12' ELEMENT)                          
         SPACE 1                                                                
DLINE    NTR1                                                                   
*                                  SHARE                                        
         EDIT  (2,NPUBSHR),(6,8(R2)),1,ALIGN=LEFT                               
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         TM    NPUBOVRD,X'80'                                                   
         BNO   *+8                                                              
         BAS   RE,FLOAT                                                         
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
*                                  HUT                                          
         EDIT  (2,NPUBHUT),(6,8(R2)),1,ALIGN=LEFT                               
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         TM    NPUBOVRD,X'40'                                                   
         BNO   *+8                                                              
         BAS   RE,FLOAT                                                         
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
*                                  RATING                                       
         EDIT  (2,NPUBRTG),(6,8(R2)),1,ALIGN=LEFT                               
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'                                                      
         TM    NPUBOVRD,X'20'                                                   
         BNO   *+8                                                              
         BAS   RE,FLOAT                                                         
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
         LA    R3,DEMOS            NOW SET UP FOR THE VPHS                      
         LA    R4,NPUBVOVR                                                      
         ZIC   R5,NDEMOS                                                        
         SPACE 1                                                                
DLINE2   MVC   GDDEMO,0(R3)        LOOK UP DEMOS                                
*DLINE2   MVC   GDDEMO,2(R3)        LOOK UP DEMOS                               
         MVI   LENGTH,0                                                         
         MVC   PERIOD,NPUBPER                                                   
         GOTO1 VGETDEM                                                          
         EDIT  (4,GDVPH),(6,8(R2)),ALIGN=LEFT                                   
         OI    4(R2),X'20'                                                      
         OC    0(2,R4),0(R4)       IS THERE AN OVERRIDE                         
         BZ    DLINE4                                                           
         EDIT  (2,0(R4)),(6,8(R2)),ALIGN=LEFT                                   
         BAS   RE,FLOAT                                                         
         SPACE 1                                                                
DLINE4   OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         LA    R3,3(R3)                                                         
         LA    R4,2(R4)                                                         
         BCT   R5,DLINE2                                                        
         SPACE 1                                                                
         LA    R0,6                CLEAR REST OF LINE                           
         ZIC   R1,NDEMOS                                                        
         SR    R0,R1                                                            
         BNP   XIT                                                              
         SPACE 1                                                                
DLINE6   MVC   8(6,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         BCT   R0,DLINE6                                                        
         B     XIT                                                              
         SPACE 1                                                                
FLOAT    NTR1                                                                   
         NI    4(R2),X'DF'         TURN OFF X'20' BIT TO INDICATE THAT          
*                                  THIS IS ALREADY AN OVERRIDE                  
         LA    R1,8(R2)                                                         
         LA    R0,5                                                             
         SPACE 1                                                                
FLOAT2   CLI   0(R1),C' '                                                       
         BNH   FLOAT4                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,FLOAT2                                                        
         B     XIT                                                              
         SPACE 1                                                                
FLOAT4   MVI   0(R1),C'*'                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SHOW DEMOS AND PLAN DETAILS                           
         SPACE 3                                                                
SHOWPLAN NTR1                                                                   
         LA    R2,PUPDEMOH                                                      
         LA    R4,PUPDEM1H                                                      
         LA    R3,DEMOS                                                         
         LA    R0,6                                                             
         SPACE 1                                                                
SHOWPL2  MVC   8(6,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
         MVC   8(6,R4),SPACES                                                   
         OI    6(R4),X'80'                                                      
         CLI   2(R3),0                                                          
         BE    SHOWPL4                                                          
         GOTO1 DEMOCON,DMCB,(0,0(R3)),(10,WORK),(C'S',DBLOCK)                   
         CLI   WORK+2,C'.'         NAD DEMO                                     
         BNE   SHOWPL3                                                          
         MVC   9(3,R4),WORK        YES/SPLIT DISPLAY                            
         MVC   8(6,R2),WORK+3                                                   
         B     *+10                                                             
SHOWPL3  MVC   8(6,R2),WORK                                                     
         SPACE 1                                                                
SHOWPL4  BAS   RE,BUMP                                                          
         ZIC   RE,0(R4)                                                         
         AR    R4,RE                                                            
         LA    R3,3(R3)                                                         
         BCT   R0,SHOWPL2                                                       
         SPACE 1                                                                
         LA    R2,PUPHUTS                                                       
         EDIT  (1,PLANHTYR),(2,0(R2))                                           
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
*              ERROR EXITS                                                      
         SPACE 3                                                                
BADQUART MVC   CONHEAD(L'QURTERR),QURTERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADOVER  MVC   CONHEAD(L'OVERERR),OVERERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
QURTERR  DC    C'NEED QUARTER NUMBER (1-4) FOR WEEKLIES'                        
OVERERR  DC    C'** ERROR ** INVALID OVERRIDE EXPRESSION'                       
         SPACE 3                                                                
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
XIT      XIT1                                                                   
         SPACE 1                                                                
XIT2     XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPE3D                                                       
         SPACE 1                                                                
DONTMOVE DS    CL20                                                             
LASTPKEY DS    CL20                                                             
VALIDKEY DS    CL20                                                             
FIRSTIN  DS    XL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NEPUP13   05/01/02'                                      
         END                                                                    

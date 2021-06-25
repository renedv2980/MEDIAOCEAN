*          DATA SET ANBLDXX    AT LEVEL 034 AS OF 05/01/02                      
*PHASE T00A7BA                                                                  
         SPACE 2                                                                
*                                                                               
* THIS ROUTINE DEALS WITH NEW MAKEGOODS                                         
*                                                                               
* P1 - PARAMETER BLOCK                                                          
*                                                                               
SPBLDMGA2 TITLE 'BLDMGA2- BUILD TABLE OF MAKEGOODS'                             
         PRINT NOGEN                                                            
BLDMGA   CSECT                                                                  
         NMOD1 WORKX-WORKD,**BMGA**,R7                                          
         USING WORKD,RC                                                         
*                                                                               
         LR    R6,R1                                                            
         USING MGABLKD,R6                                                       
         L     R9,MGAACOM                                                       
         USING COMFACSD,R9                                                      
         MVC   USERRD,4(RD)        SAVE LINK BACK TO USER                       
         USING TSARD,R8                                                         
         LA    R8,TSARBLK                                                       
         XC    TSARBLK,TSARBLK                                                  
*                                                                               
         MVI   MGAERR,0                                                         
         MVC   DATADISP,=H'24'                                                  
         CLI   MGAACT,0            BUILD THE TABLE                              
         BNE   M10                                                              
         BAS   RE,MGABLDT                                                       
         B     MGAX                                                             
*                                                                               
M10      CLI   MGAACT,MGAQADD      ADD AN ENTRY                                 
         BNE   M20                                                              
         BAS   RE,ADDENTRY                                                      
         B     MGAX                                                             
*                                                                               
M20      CLI   MGAACT,MGAQDEL      DELETE AN ENTRY                              
         BNE   M30                                                              
         BAS   RE,DELENTRY                                                      
*                                                                               
M30      CLI   MGAACT,MGAQCOD      GET NEXT AVAILABLE CODE                      
         BNE   M40                                                              
         BAS   RE,GETCODE                                                       
*                                                                               
M40      CLI   MGAACT,MGAQFND      FIND THIS ENTRY                              
         BNE   M50                                                              
         BAS   RE,FNDENTRY                                                      
*                                                                               
M50      CLI   MGAACT,MGAQNXT      FIND NEXT ENTRY                              
         BNE   M60                                                              
         BAS   RE,NXTENTRY                                                      
*                                                                               
M60      CLI   MGAACT,MGAQRDH      READ HIGH                                    
         BNE   M70                                                              
         BAS   RE,RDHI                                                          
*                                                                               
M70      CLI   MGAACT,MGAQBLN      BUILD A TABLE FOR A SINGLE BUY LINE          
         BNE   M80                                                              
         BAS   RE,BLINE                                                         
*                                                                               
M80      CLI   MGAACT,MGAQGET      GET AN ENTRY - MGATSNUM SET                  
         BNE   M90                                                              
         BAS   RE,GETENTRY                                                      
*                                                                               
M90      CLI   MGAACT,MGAQTOT      DO TOTALS FOR TABLE ENTRY                    
         BNE   M100                                                             
         BAS   RE,TOTAL                                                         
*                                                                               
M100     CLI   MGAACT,MGAQTRNS     TRANSLATE CODES                              
         BNE   M110                                                             
         BAS   RE,TRANSLAT                                                      
*                                                                               
M110     CLI   MGAACT,MGAQPRNT     SET UP A PRINT LINE                          
         BNE   M120                                                             
         BAS   RE,SETPRNT                                                       
*                                                                               
M120     DS    0H                                                               
*                                                                               
MGAX     B     MGAXIT                                                           
         EJECT                                                                  
*                                                                               
*        BUILD THE TABLE                                                        
*                                                                               
MGABLDT  NTR1                                                                   
         OC    MGATAB,MGATAB       IS THERE A TABLE                             
         BZ    MGA05                                                            
         L     R1,MGATAB           A(BEGINING OF TABLE)                         
         LH    R2,MGATABLN                                                      
         TM    MGAOPT,MGOFULN      USE FULL WORD FOR LENGTH                     
         BNO   *+8                                                              
         L     R2,MGATABLF                                                      
         AR    R1,R2                                                            
         ST    R1,ATABLEX                                                       
         B     MGA10                                                            
*                                                                               
MGA05    XC    TSARBLK,TSARBLK                                                  
         CLI   MGTSINIT,C'Y'       WAS TSAR INITIALIZED                         
         BE    MGA10                                                            
         MVI   MGTSINIT,C'Y'                                                    
*                                                                               
         XC    GMISSTOT,GMISSTOT                                                
         XC    GMGTOT,GMGTOT                                                    
         XC    GMISSRTG,GMISSRTG                                                
         XC    GMGRTG,GMGRTG                                                    
*                                                                               
         MVI   BYTE,TSAINI         INITIALIZE TSAR                              
         BAS   RE,CALLTSAR                                                      
         TM    TSINDS,TSIINIOK                                                  
         BNZ   *+6                                                              
         DC    H'0'                NO MORE SPACE ON TEMPEST FILE                
*                                                                               
MGA10    OC    MGAKEY,MGAKEY       ALREADY READ A REC                           
         BNZ   MGA20                                                            
         LA    R3,MGAKEY                                                        
         USING BUYRECD,R3                                                       
         MVC   BUYKAM,MGAAGMD      AGY/MEDIA                                    
         MVC   BUYKCLT,MGACLT      CLIENT                                       
         MVC   BUYKPRD,MGAPRD      PRODUCT                                      
         MVC   BUYMSTA,MGASTA      MARKET/STATION                               
         MVC   BUYKEST,MGAEST                                                   
*                                                                               
MGA20    MVC   KEY,MGAKEY                                                       
         BAS   RE,HIGH                                                          
         B     MGA40                                                            
*                                                                               
MGA30    BAS   RE,SEQ                                                           
*                                                                               
MGA40    MVC   MGAKEY,KEY          SET LAST KEY READ                            
         CLC   KEY(BUYKBUY-BUYREC),KEYSAVE                                      
         BNE   MGA50                                                            
         BAS   RE,GETREC           GET THE RECORD                               
         BAS   RE,PROCBUY          PROCESS THE BUY                              
         B     MGA30                                                            
*                                                                               
MGA50    BAS   RE,WRAPUP                                                        
*                                                                               
MGABX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        BUILD A TABLE FOR A SINGLE BUY LINE                                    
*                                                                               
BLINE    NTR1                                                                   
         MVI   MGAERR,MGAQRMIS                                                  
         OC    MGABUY,MGABUY       MAKE SURE A(BUY) IS PASSED                   
         BZ    BLNX                                                             
         OC    MGATAB,MGATAB       AND A(TABLE)                                 
         BZ    BLNX                                                             
*                                                                               
         L     R1,MGATAB           A(TABLE)                                     
         LH    R2,MGATABLN                                                      
         TM    MGAOPT,MGOFULN      USE FULL WORD FOR LENGTH                     
         BNO   *+8                                                              
         L     R2,MGATABLF                                                      
         AR    R1,R2                                                            
         ST    R1,ATABLEX          SET A(END OF TABLE)                          
*                                                                               
         L     R4,MGATAB           A(TABLE)                                     
         TM    MGAOPT,MGOPENTB     PUT AT END OF TABLE                          
         BNO   BL20                                                             
*                                                                               
BL10     OC    0(MGERECL,R4),0(R4) YES - FIND END OF TABLE                      
         BZ    BL30                                                             
         LA    R4,MGERECL(R4)      BUMP TO NEXT ENTRY                           
         CR    R4,R1               R1 POINTS TO END OF TABLE                    
         BNL   TABFULER                                                         
         B     BL10                                                             
*                                                                               
BL20     XC    0(MGERECL,R4),0(R4)                                              
*                                                                               
BL30     ST    R4,ANENTRY          A(NEXT ENTRY)                                
*                                                                               
         MVI   MGAERR,0                                                         
         MVC   MGAIO,MGABUY        SET A(IO AREA)                               
         BAS   RE,PROCBUY                                                       
         TM    MGAOPT,MGOPENTB     DON'T DO TOTALS YET                          
         BO    BLNX                                                             
         BAS   RE,WRAPUP                                                        
*                                                                               
BLNX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        JUST DO TOTALS                                                         
*                                                                               
TOTAL    NTR1                                                                   
         L     R1,MGATAB           A(TABLE)                                     
         LH    R2,MGATABLN                                                      
         TM    MGAOPT,MGOFULN      USE FULL WORD FOR LENGTH                     
         BNO   *+8                                                              
         L     R2,MGATABLF                                                      
         AR    R1,R2                                                            
         ST    R1,ATABLEX          SET A(END OF TABLE)                          
*                                                                               
         L     R4,MGATAB           A(TABLE)                                     
         TM    MGAOPT,MGOPENTB     PUT AT END OF TABLE                          
         BNO   TO20                                                             
*                                                                               
TO10     OC    0(MGERECL,R4),0(R4) YES - FIND END OF TABLE                      
         BZ    TO30                                                             
         LA    R4,MGERECL(R4)      BUMP TO NEXT ENTRY                           
         CR    R4,R1               R1 POINTS TO END OF TABLE                    
         BNL   TABFULER                                                         
         B     TO10                                                             
*                                                                               
TO20     XC    0(MGERECL,R4),0(R4)                                              
*                                                                               
TO30     ST    R4,ANENTRY          A(NEXT ENTRY)                                
         BAS   RE,WRAPUP                                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        WRAP UP TOTALS & SORT FOR OFFLINE                                      
*                                                                               
WRAPUP   NTR1                                                                   
         BAS   RE,TOTENTRY                                                      
         OC    MGATAB,MGATAB       IS THERE A TABLE                             
         BZ    WRX                                                              
         LH    R4,MGACNT                                                        
         LTR   R4,R4                                                            
         BZ    WRX                                                              
         GOTO1 CXSORT,DMCB,MGATAB,(R4),MGERECL,MGEKEYL,0                        
         BAS   RE,TABTOTAL         DO TOTALS FOR TABLE                          
         LH    R4,MGACNT                                                        
         LTR   R4,R4                                                            
         BNZ   *+6                 SORT AGAIN                                   
         DC    H'0'                                                             
         GOTO1 CXSORT,DMCB,MGATAB,(R4),MGERECL,MGEKEYL,0                        
*                                                                               
WRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS A BUY RECORD                                                   
*                                                                               
PROCBUY  NTR1                                                                   
         MVI   BUYCIND,0                                                        
         MVI   BUYSTAT,0                                                        
         L     R3,MGAIO                                                         
         USING BUYRECD,R3                                                       
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
*                                                                               
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    PB5A                                                             
* BDCIND IS CHARACTER                                                           
         CLI   BDCIND,C'P'         TEST NTP RATE                                
         BNE   *+8                                                              
         OI    BUYSTAT,MGESTAT_NTP                                              
*                                                                               
         TM    BDCIND2,BDC2NEG     TEST MINUS RATE                              
         BZ    PB5C                                                             
         OI    BUYCIND,X'01'       (ONLY SAVES THAT IT WAS NEGATIVE)            
         B     PB5B                                                             
* BDCIND IS A BITMASK                                                           
PB5A     MVC   BUYCIND,BDCIND      SAVE COST IND                                
         CLI   BUYCIND,0           TEST NTP RATE                                
         BNE   *+8                                                              
         OI    BUYSTAT,MGESTAT_NTP                                              
*                                                                               
         TM    BDCIND,X'01'        TEST MINUS RATE                              
         BZ    PB5C                                                             
*                                                                               
PB5B     LNR   R0,R0                                                            
*                                                                               
PB5C     ST    R0,BUYCOST          SAVE BUYLINE COST                            
*                                                                               
         CLC   =C'DF',BUYALPHA     ONLY DF HAS NTP TRADE RATE                   
         BE    *+8                                                              
         NI    BUYSTAT,X'FF'-MGESTAT_NTP                                        
*                                                                               
         XC    LASTDATE,LASTDATE   CLEAR LAST DATE                              
         MVI   SPOTNUM,0           RESET SPOT NUMBER                            
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
PB10     L     R3,MGAIO                                                         
         LA    R3,24(R3)                                                        
         USING REGELEM,R3                                                       
         BAS   RE,NEXTEL           FIND 0B/0C ELEMENT                           
         BNE   PB140                                                            
*                                                                               
PB20     ST    R3,SAVER3                                                        
         SR    R0,R0                                                            
         MVI   BCODE,0                                                          
*                                                                               
         XC    PRD1(4),PRD1                                                     
         TM    MGAOPT,MGOFLTDT     FILTER BY DATE                               
         BNO   PB30                                                             
         CLC   RDATE,MGSFLTDT      START DATE                                   
         BL    PB120                                                            
         CLC   RDATE,MGEFLTDT      END DATE                                     
         BH    PB120                                                            
*                                                                               
PB30     CLC   RDATE,LASTDATE      SAME DATE                                    
         BE    *+8                                                              
         MVI   SPOTNUM,0           NO - RESET SPOT NUMBER                       
         MVC   LASTDATE,RDATE                                                   
*                                                                               
         TM    RSTATUS,X'80'       IF MINUS                                     
         BO    PB120               SKIP                                         
         ZIC   R1,SPOTNUM          INC SPOT NUMBER                              
         LA    R1,1(R1)                                                         
         STC   R1,SPOTNUM                                                       
*                                                                               
         MVI   BYTE,0              SET ENTRY TYPE TO MISSED                     
         CLI   1(R3),X'0E'         MAKE SURE THAT IT'S ALLOCATED                
         BL    PB120                                                            
         MVC   PRD1,10(R3)                                                      
         MVC   SLN1,11(R3)                                                      
         CLI   1(R3),X'0E'         IS THERE A PRD2                              
         BE    PB40                                                             
         MVC   PRD2,14(R3)                                                      
         MVC   SLN2,15(R3)                                                      
*                                                                               
PB40     DS    0H                                                               
         CLI   MGAPRD,X'FF'                                                     
         BE    PB42                                                             
         CLC   PRD1,MGAPRD         REQUEST PRD MATCH ANY ALLOC PRD              
         BE    PB42                                                             
         CLC   PRD2,MGAPRD                                                      
         BNE   PB120                                                            
*                                                                               
PB42     MVC   LASTCOST,BUYCOST                                                 
         TM    RSTATUS,X'20'       TEST RATE OVERRIDE                           
         BZ    PB44                                                             
         SR    R0,R0                                                            
         ICM   R0,7,RPCOST                                                      
         TM    BUYCIND,X'01'       TEST MINUS RATE                              
         BZ    *+6                                                              
         LNR   R0,R0                                                            
         ST    R0,LASTCOST                                                      
*                                                                               
PB44     TM    BUYSTAT,MGESTAT_NTP TRADE BUY = 0 COST BUT NOT A BONUS           
         BZ    *+14                                                             
         XC    LASTCOST,LASTCOST   COST=$0                                      
         B     PB45                                                             
*                                                                               
         OC    LASTCOST,LASTCOST   COST=$0                                      
         BNZ   PB45                                                             
         TM    RSTATUS,X'40'       TEST MINUSSED AND                            
         BNO   *+12                                                             
         TM    RSTATUS,X'02'       MADE GOOD ON ANOTHER LINE                    
         BO    PB45                YES - IGNORE BONUS                           
         L     RE,MGAIO                                                         
         LA    RE,BDMGDATE-BUYRECD(RE)                                          
         CLI   0(RE),X'C0'         TEST THIS IS A NEW MAKEGOOD LINE             
         BH    PB45                YES - IGNORE BONUS                           
*                                                                               
         TM    MGAOPT,MGONONC      SKIP NO CHARGE                               
         BO    PB120                                                            
         MVI   BCODE,X'FD'         BONUS                                        
         MVI   BYTE,1              MAKEGOOD                                     
         B     PB70                                                             
*                                                                               
PB45     TM    RSTATUS,X'40'       TEST MINUSSED                                
         BNO   PB50                NO                                           
         MVI   BCODE,X'FF'         SET PRE-EMPTED                               
         TM    RSTATUS,X'02'       IS IT MADE GOOD ON ANOTHER LINE              
         BO    PB47                YES                                          
         L     RE,MGAIO            POINT TO BUYREC                              
         LA    RE,BDMGDATE-BUYRECD(RE)                                          
         CLI   0(RE),X'C0'         TEST THIS IS A NEW MAKEGOOD LINE             
         BH    PB120               YES - IGNORE PRE-EMPTION                     
         B     PB70                                                             
*                                                                               
PB47     MVC   BCODE,13(R3)        MOVE MG CODE                                 
         NI    BCODE,X'3F'         DROP OTHER BITS                              
         CLI   BCODE,0             IF 0, SET OLD STYLE MG                       
         BNE   *+8                                                              
         MVI   BCODE,X'FE'                                                      
         B     PB70                                                             
*                                                                               
PB50     L     R3,MGAIO                                                         
         USING BUYRECD,R3                                                       
*                                                                               
PB60     CLI   BDMGDATE,X'C0'      SEE IF NEW MAKEGOOD                          
         BH    PB65                                                             
         OC    BDMGDATE,BDMGDATE   SEE IF OLD STYLE MAKEGOOD                    
         BZ    PB120                                                            
         MVC   CODE,=C'MG'                                                      
         B     *+10                                                             
*                                                                               
PB65     MVC   CODE,BDMGDATE                                                    
         MVI   BYTE,1              SET ENTRY TYPE TO MAKEGOOD                   
*                                                                               
PB70     CLI   MGAPRD,X'FF'                                                     
         BE    PB75                                                             
         CLC   PRD1,MGAPRD                                                      
         BE    PB75                                                             
         CLC   PRD2,MGAPRD                                                      
         BNE   PB120                                                            
*                                                                               
PB75     TM    MGAOPT,MGOFLTPR     FILTER BY PRODUCT                            
         BNO   PB110                                                            
         BAS   RE,CHKPRFLT         GO CHECK PRODUCT FILTERS                     
         BNE   PB120                                                            
*                                                                               
PB110    BAS   RE,TRANSCD          TRANSLATE CODE X'01-X'3C'TO A0-F9            
*                                                                               
PB115    BAS   RE,BLDENTRY         YES - BUILD ENTRY TO TABLE                   
         BAS   RE,BLDTABLE         PUT ENTRY TO TABLE/TSAR/GO TO HOOK           
*                                                                               
PB120    L     R3,SAVER3                                                        
         BAS   RE,NEXTEL           GO FIND NEXT ELEMENT                         
         BE    PB20                                                             
*                                                                               
PB140    BAS   RE,TSTSPCL          TEST FOR SPECIAL CASE                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CHECK PRODUCT FILTERS                                                  
*                                                                               
CHKPRFLT NTR1                                                                   
         CLI   MGFLTPRD,X'FF'      ALL PRODUCTS                                 
         BE    CF10                                                             
         CLI   MGFLTPRD,0          ALL PRODUCTS                                 
         BE    CF10                                                             
         CLC   MGFLTPRD,PRD1       DOES PRODUCT MATCH                           
         BNE   CFNO                                                             
*                                                                               
CF10     CLI   MGFLTPG,X'FE'       DON'T FILTER ON PIGGYBACKS                   
         BE    CFYES                                                            
         CLI   MGFLTPG,0           EXCLUDE PIGGYBACKS                           
         BNE   CF20                                                             
         CLI   PRD2,0              IS THIS A PIGGYBACK                          
         BE    CFYES               NO - KEEP IT                                 
         B     CFNO                                                             
*                                                                               
CF20     CLI   MGFLTPG,X'FF'       ONLY PIGGYBACKS                              
         BNE   CF30                                                             
         CLI   PRD2,0              IS THIS A PIGGYBACK                          
         BNE   CFYES               YES- KEEP IT                                 
         B     CFNO                                                             
*                                                                               
CF30     CLC   MGFLTPG,PRD2       DOES PIGGYBACK MATCH                          
         BNE   CFNO                                                             
         B     CFYES                                                            
*                                                                               
CFYES    B     YES                                                              
CFNO     B     NO                                                               
         EJECT                                                                  
*====================================================================*          
* TEST IF ALL SPOTS ON A MAKEGOOD LINE HAVE BEEN MISSED              *          
* IF SO, PRINT 'MISSED' FOR THE MAKEGOOD LINE                        *          
*====================================================================*          
         SPACE 1                                                                
TSTSPCL  NTR1                                                                   
         L     R3,MGAIO                                                         
         USING BUYRECD,R3                                                       
         CLI   BDMGDATE,X'C1'      SEE IF NEW MAKEGOOD                          
         BL    TSX                                                              
         MVC   CODE,BDMGDATE                                                    
         MVI   BYTE,1              SET ENTRY TYPE TO MAKEGOOD                   
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
         LA    R3,24(R3)                                                        
         USING REGELEM,R3                                                       
*                                                                               
TS10     BAS   RE,NEXTEL           FIND 0B/OC ELEMENT                           
         BNE   TS20                NO - DIDN'T FIND ANY THAT RAN                
         TM    RSTATUS,X'C0'       TEST MINUS OR MINUSSED                       
         BZ    TSX                 NO - DONE                                    
         B     TS10                ELSE TRY AGAIN                               
*                                                                               
TS20     MVC   LASTDATE,=X'FFFF'   SET HIGH DATE                                
         MVI   SPOTNUM,1                                                        
*                                                                               
         BAS   RE,TRANSCD          TRANSLATE CODE X'01-X'3C'TO A0-F9            
         BAS   RE,BLDENTRY         YES - BUILD ENTRY TO TABLE                   
* NEXT INSTRUCTION MUST BE CAREFUL NOT TO CLEAR BCODE                           
         XC    ENTRY+MGEKEYL+1(L'ENTRY-MGEKEYL-1),ENTRY+MGEKEYL+1               
         BAS   RE,BLDTABLE         PUT ENTRY TO TABLE/TSAR/GO TO HOOK           
*                                                                               
TSX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        TRANSLATE CODE                                                         
*                                                                               
TRANSLAT NTR1                                                                   
         XC    ENTRY,ENTRY         CLEAR ENTRY                                  
         MVI   BCODE,0                                                          
         XC    CODE,CODE                                                        
         MVC   MGAIO,MGABUY        SET A(IO AREA)                               
         L     R3,MGAIO                                                         
         USING BUYRECD,R3                                                       
*                                                                               
         OC    MGAELEM,MGAELEM     IS A(ELEMENT PASSED)                         
         BNZ   TRNS10                                                           
*                                                                               
         CLI   BDMGDATE,X'C0'      SEE IF NEW MAKEGOOD                          
         BH    TRNS05                                                           
         OC    BDCOST,BDCOST                                                    
         BNZ   TRNS02                                                           
         MVI   BCODE,X'FD'         BONUS                                        
         MVI   BYTE,1              SET ENTRY TYPE TO MAKEGOOD                   
         B     TRNS30                                                           
*                                                                               
TRNS02   OC    BDMGDATE,BDMGDATE   SEE IF OLD STYLE MAKEGOOD                    
         BZ    TRNSX                                                            
         MVC   CODE,=C'MG'                                                      
         B     *+10                                                             
*                                                                               
TRNS05   MVC   CODE,BDMGDATE                                                    
         MVI   BYTE,1              SET ENTRY TYPE TO MAKEGOOD                   
         B     TRNS30                                                           
*                                                                               
         USING REGELEM,R3                                                       
TRNS10   L     R3,MGAELEM                                                       
         ST    R3,SAVER3                                                        
         TM    RSTATUS,X'40'       TEST MINUSSED                                
         BNO   TRNSX               NO                                           
         CLI   1(R3),X'0E'         MAKE SURE THAT IT'S ALLOCATED                
         BL    TRNSX                                                            
         MVI   BCODE,X'FF'         SET PRE-EMPTED                               
         TM    RSTATUS,X'02'       TEST MADEGOOD                                
         BZ    TRNS30              NO                                           
         MVC   BCODE,13(R3)        MOVE GROUP CODE                              
         NI    BCODE,X'3F'         DROP OTHER BITS                              
         CLI   BCODE,0             TEST NEW MG CODE                             
         BNE   *+8                                                              
         MVI   BCODE,X'FE'         SET OLD STYLE MG                             
*                                                                               
TRNS30   BAS   RE,TRANSCD          TRANSLATE CODE X'01-X'3C'TO A0-F9            
         BAS   RE,BLDENTRY         YES - BUILD ENTRY TO TABLE                   
         MVC   MGAENTRY,ENTRY                                                   
*                                                                               
TRNSX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        TRANSLATE CODE FROM X'01' - X'3C' TO A0 - F9                           
*                                                                               
TRANSCD  NTR1                                                                   
         CLI   BCODE,X'FD'         BONUS                                        
         BNE   TC01                                                             
         MVC   CODE,=C'NC'                                                      
         B     TCX                                                              
*                                                                               
TC01     CLI   BYTE,1              IS THIS MAKEGOOD                             
         BE    TC30                                                             
         CLI   BCODE,X'FF'         MISSED BUT NOT MADEGOOD                      
         BNE   TC05                                                             
         MVC   CODE,=C'PR'                                                      
         B     TCX                                                              
*                                                                               
TC05     CLI   BCODE,X'FE'         OLD STYLE MAKEGOOD                           
         BNE   TC07                                                             
         MVC   CODE,=C'MG'                                                      
         B     TCX                                                              
*                                                                               
TC07     ZIC   R1,BCODE            X'01'- X'3C'                                 
         LA    R4,C'A'             START AT 'A'                                 
*                                                                               
TC10     CH    R1,=H'10'                                                        
         BNH   TC20                                                             
         LA    R4,1(R4)                                                         
         SH    R1,=H'10'                                                        
         B     TC10                                                             
*                                                                               
TC20     STC   R4,CODE                                                          
         BCTR  R1,0                                                             
         STC   R1,CODE+1                                                        
         OI    CODE+1,X'F0'        SET TO NUMBER                                
         B     TCX                                                              
*                                                                               
TC30     CLC   CODE,=C'MG'         OLD STYLE MAKEGOOD                           
         BNE   TC35                                                             
         MVI   BCODE,X'FE'                                                      
         B     TCX                                                              
*                                                                               
TC35     ZIC   R1,CODE             PUT ALPHA IN R1                              
         LA    R4,C'A'                                                          
         SR    R1,R4                                                            
         LA    R2,1                                                             
*                                                                               
TC40     CH    R1,=H'0'                                                         
         BE    TC50                                                             
         AH    R2,=H'10'           ADD 10 FOR EACH LETTER                       
         BCTR  R1,0                                                             
         B     TC40                                                             
*                                                                               
TC50     MVC   HALF(1),CODE+1                                                   
         NI    HALF,X'0F'                                                       
         ZIC   R1,HALF                                                          
         AR    R1,R2                                                            
         STC   R1,BCODE                                                         
*                                                                               
TCX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        BUILD AN ENTRY TO THE TABLE                                            
*                                                                               
BLDENTRY NTR1                                                                   
         XC    ENTRY,ENTRY         CLEAR ENTRY                                  
         MVC   SVELCDS,ELCDLO      SAVE PREVIOUS ELCODES                        
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         CLI   MGAACT,MGAQTRNS     IF TRANSLATE CODES                           
         BE    BE01                DON'T COUNT                                  
         LH    R1,MGACNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         LA    R1,1(R1)                                                         
         STH   R1,MGACNT                                                        
*                                                                               
BE01     L     R3,MGAIO                                                         
         USING BUYRECD,R3                                                       
         MVC   MGELINE,BUYKBUY       LINE NUMBER                                
         MVC   MGESLN,BDSEC          SPOT LENGTH                                
         MVC   MGETIME,BDTIMST       TIME                                       
         MVC   MGECOST,LASTCOST                                                 
         MVC   MGEDAYPT,BDDAYPT      DAYPART                                    
         MVC   MGEPGMNM,BDPROGRM     PROGRAM                                    
         MVC   MGESTAT,BUYSTAT       SAVE STATUS FLAGS                          
*                                                                               
         BAS   RE,GETRTG                                                        
         MVC   MGERTG,RATING       RATING                                       
         MVC   MGERTGB,RATINGB     BINARY RATING                                
*                                                                               
         CLI   BYTE,1              IS THIS A MAKEGOOD LINE                      
         BNE   BE05                                                             
         L     R3,MGAIO                                                         
         MVI   ELCDLO,X'29'        YES LOOK FOR EXCEPTION ELEMENT               
         MVI   ELCDHI,X'29'                                                     
         L     R3,MGAIO                                                         
         LA    R3,24(R3)                                                        
         BAS   RE,NEXTEL           IN RECORD                                    
         BE    BE07                                                             
         B     BE10                                                             
*                                                                               
BE05     L     R3,SAVER3           ELSE - MISSED EXCEPTION MUST BE              
*                                                                               
BE06     CLI   0(R3),0             END OF RECORD                                
         BE    BE10                                                             
         ZIC   R1,1(R3)            RIGHT AFTER THE 0C ELEMENT                   
         LTR   R1,R1                                                            
         BZ    BE10                                                             
         AR    R3,R1                                                            
         CLI   0(R3),X'0B'         DID WE REACH NEXT X'0B' ELEMENT              
         BE    BE10                YES - DONE                                   
         CLI   0(R3),X'19'                                                      
         BNE   BE06                                                             
         EJECT                                                                  
BE07     MVC   MGESTA,2(R3)        STATION EXCEPTION                            
         ZIC   R1,CODE             CHANGE A1 TO U1, B3 TO V3 ETC                
         AH    R1,=H'35'           DIFFERENCE BETWEEN U1 & A1                   
         STC   R1,CODE                                                          
*                                                                               
BE10     MVC   MGETYPE,BYTE                                                     
         MVC   MGEDATE,LASTDATE    SET DATE                                     
         MVC   MGESPNUM,SPOTNUM    SPOT NUMBER                                  
         MVC   MGECODE,CODE        MAKEGOOD CODE                                
         MVC   MGAECOD,BCODE       BINARY CODE                                  
         MVC   MGEPRD1,PRD1        PRODUCT 1                                    
         MVC   MGESLN1,SLN1        SPOT LEN 1                                   
         MVC   MGEPRD2,PRD2        PRODUCT 2                                    
         MVC   MGESLN2,SLN2        SPOT LEN 2                                   
         MVC   ELCDLO(2),SVELCDS     RESTORE ELCODES                            
*                                                                               
BEX      B     XIT                                                              
         EJECT                                                                  
*        PUT OUT TOTAL ENTRY                                                    
*                                                                               
TOTENTRY NTR1                                                                   
         OC    MGATAB,MGATAB       IF AN AREA IS PASSED TO US                   
         BNZ   TEX                    SKIP                                      
         BAS   RE,CLRACCUM         CLEAR ACCUMULATORS                           
*                                                                               
         XC    THISCODE,THISCODE                                                
         XC    THISSTA,THISSTA                                                  
*                                                                               
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
*                                                                               
         XC    TSRNUM,TSRNUM                                                    
         XC    MGECODE(MGERECL),MGECODE            CLR REC                      
         MVI   BYTE,TSARDH         READ HIGH                                    
         B     *+8                                                              
*                                                                               
TE10     MVI   BYTE,TSANXT         READ HIGH                                    
         BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BO    TE60                                                             
*                                                                               
         CLC   MGECODE,THISCODE    TEST SAME CODE                               
         BNE   *+14                                                             
         CLC   MGESTA,THISSTA      TEST SAME STATION                            
         BE    TE30                                                             
         OC    THISCODE,THISCODE   SKIP FIRST TIME                              
         BZ    TE30                                                             
         CLC   MGECODE,=X'FEFE'    IS THIS THE GRAND TOTAL RECORD               
         BE    TE70                                                             
         BAS   RE,ADDTOT           ADD TOTAL RECORD                             
         XC    MISSTOT,MISSTOT     CLEAR ACCUMULATORS                           
         XC    MGTOT,MGTOT                                                      
         XC    MISSRTG,MISSRTG                                                  
         XC    MGRTG,MGRTG                                                      
         B     TE40                                                             
*                                                                               
TE30     DS    0H                                                               
         CLI   MGETYPE,X'FE'       IS THIS THE TOTAL RECORD                     
         BNE   TE40                                                             
         MVI   BYTE3,C'Y'          THIS CODE'S TOTAL DOES EXIST                 
         BAS   RE,PUTTOT           PUT TOTAL RECORD                             
         B     TE50                                                             
*                                                                               
TE40     MVI   BYTE3,C'N'          THIS CODE'S TOTAL DOESN'T EXIST              
         BAS   RE,ACCUM            ACCUMULATE THIS ENTRY                        
*                                                                               
TE50     B     TE10                                                             
         EJECT                                                                  
TE60     OC    THISCODE,THISCODE   IF THERE WAS NOTHING - EXIT                  
         BZ    TEX                                                              
         BAS   RE,ADDTOT           ADD LAST TOTAL RECORD                        
         MVI   BYTE3,C'N'                                                       
         MVC   THISCODE,=X'FEFE'   SET FOR GRAND TOTAL                          
         XC    THISSTA,THISSTA                                                  
         MVC   MISSTOT,GMISSTOT    SET GRAND TOTAL                              
         MVC   MGTOT,GMGTOT                                                     
         MVC   MISSRTG,GMISSRTG                                                 
         MVC   MGRTG,GMGRTG                                                     
         BAS   RE,ADDTOT           ADD TOTAL RECORD                             
         B     TEX                                                              
*                                                                               
TE70     MVC   MISSTOT,GMISSTOT    SET GRAND TOTAL                              
         MVC   MGTOT,GMGTOT                                                     
         MVC   MISSRTG,GMISSRTG                                                 
         MVC   MGRTG,GMGRTG                                                     
         BAS   RE,PUTTOT           PUT GRAND TOTAL RECORD                       
*                                                                               
TEX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PUT A TOTAL RECORD                                                     
*                                                                               
TABTOTAL NTR1                                                                   
         XC    GMISSTOT,GMISSTOT                                                
         XC    GMGTOT,GMGTOT                                                    
         XC    GMISSRTG,GMISSRTG                                                
         XC    GMGRTG,GMGRTG                                                    
*                                                                               
         BAS   RE,CLRACCUM                                                      
         XC    THISCODE,THISCODE                                                
         L     R4,MGATAB           A(TABLE)                                     
         USING MGENTRYD,R4                                                      
         OC    0(MGERECL,R4),0(R4)   IF THE TABLE IS EMPTY                      
         BZ    TTX                      EXIT                                    
*                                                                               
TT10     OC    0(MGERECL,R4),0(R4) END OF TABLE                                 
         BZ    TT30                                                             
         CLI   MGETYPE,X'FE'       DID WE REACH THE TOTALS                      
         BE    TT30                                                             
         CLC   MGECODE,THISCODE    SAME CODE                                    
         BNE   *+14                                                             
         CLC   MGESTA,THISSTA      SAME STATION                                 
         BE    TT20                                                             
         OC    THISCODE,THISCODE   SKIP FIRST TIME                              
         BZ    TT20                                                             
         BAS   RE,TABTOT                                                        
*                                                                               
TT20     BAS   RE,ACCUM            ACCUMULATE THIS ENTRY                        
         LA    R4,MGERECL(R4)      BUMP TO NEXT POSITION                        
         B     TT10                                                             
*                                                                               
TT30     BAS   RE,TABTOT           ADD LAST TOTAL                               
         MVC   THISCODE,=X'FEFE'   SET GRAND TOTAL                              
         XC    THISSTA,THISSTA                                                  
         MVC   MISSTOT,GMISSTOT                                                 
         MVC   MGTOT,GMGTOT                                                     
         MVC   MISSRTG,GMISSRTG                                                 
         MVC   MGRTG,GMGRTG                                                     
         BAS   RE,TABTOT           ADD GRAND TOTAL                              
*                                                                               
TTX      B     XIT                                                              
         EJECT                                                                  
*        ACCUMULATE AMOUNTS IN THIS ENTRY                                       
*                                                                               
ACCUM    NTR1                                                                   
         MVC   THISCODE,MGECODE                                                 
         MVC   THISSTA,MGESTA                                                   
*                                                                               
         L     R1,MGECOST          COST                                         
         SR    R2,R2                                                            
         ICM   R2,15,MGERTGB       BINARY RATING                                
         CLI   MGETYPE,0                                                        
         BNE   AC40                                                             
         L     R5,MISSTOT          MISSED TOTALS                                
         AR    R5,R1                                                            
         ST    R5,MISSTOT                                                       
         L     R5,GMISSTOT                                                      
         AR    R5,R1                                                            
         ST    R5,GMISSTOT                                                      
*                                                                               
         L     R5,MISSRTG                                                       
         AR    R5,R2                                                            
         ST    R5,MISSRTG                                                       
         L     R5,GMISSRTG                                                      
         AR    R5,R2                                                            
         ST    R5,GMISSRTG                                                      
         B     ACX                                                              
*                                                                               
AC40     L     R5,MGTOT            MAKEGOOD TOTALS                              
         AR    R5,R1                                                            
         ST    R5,MGTOT                                                         
         L     R5,GMGTOT                                                        
         AR    R5,R1                                                            
         ST    R5,GMGTOT                                                        
*                                                                               
         L     R5,MGRTG                                                         
         AR    R5,R2                                                            
         ST    R5,MGRTG                                                         
         L     R5,GMGRTG                                                        
         AR    R5,R2                                                            
         ST    R5,GMGRTG                                                        
*                                                                               
ACX      B     XIT                                                              
         EJECT                                                                  
*        ADD A TOTAL RECORD                                                     
*                                                                               
ADDTOT   NTR1                                                                   
         CLI   BYTE3,C'Y'          WAS THE TOTAL RECORD WRITTEN BACK            
         BE    ADX                                                              
         MVC   ENTRY2,ENTRY                                                     
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         XC    ENTRY,ENTRY                                                      
         MVC   MGECODE,THISCODE                                                 
         MVC   MGESTA,THISSTA                                                   
         MVI   MGETYPE,X'FE'                                                    
*                                                                               
         MVC   MGETMISS,MISSTOT                                                 
         MVC   MGETMG,MGTOT                                                     
         MVC   MGEMSRTG,MISSRTG                                                 
         MVC   MGEMGRTG,MGRTG                                                   
         EDIT  MISSRTG,(4,MGEMSRTE),1                                           
         EDIT  MGRTG,(4,MGEMGRTE),1                                             
*                                                                               
         MVI   BYTE,TSAADD         ADD RECORD TO TSAR                           
*                                                                               
         BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSEDUP       DUPLICATE KEY                                
         BNO   *+6                                                              
         DC    H'0'                NO MORE SPACE ON TEMPEST FILE                
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BNO   *+8                                                              
         MVI   MGAERR,MGAQEOF                                                   
*                                                                               
         MVC   ENTRY,ENTRY2                                                     
         MVI   BYTE,TSARDH         RE-READ LAST RECORD READ                     
         BAS   RE,CALLTSAR                                                      
*                                                                               
ADX      B     XIT                                                              
         EJECT                                                                  
*        ADD TOTAL ENTRY                                                        
*                                                                               
TABTOT   NTR1                                                                   
         L     R1,ANENTRY          SET A(NEXT ENTRY)                            
         XC    ENTRY,ENTRY                                                      
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         MVC   MGECODE,THISCODE                                                 
         MVC   MGESTA,THISSTA                                                   
         MVI   MGETYPE,X'FE'                                                    
*                                                                               
         MVC   MGETMISS,MISSTOT                                                 
         MVC   MGETMG,MGTOT                                                     
         MVC   MGEMSRTG,MISSRTG                                                 
         MVC   MGEMGRTG,MGRTG                                                   
         EDIT  MISSRTG,(4,MGEMSRTE),1                                           
         EDIT  MGRTG,(4,MGEMGRTE),1                                             
*                                                                               
         MVC   0(MGERECL,R1),ENTRY                                              
         LH    R4,MGACNT           INCREMENT NUMBER OF ENTRIES IN TABLE         
         LA    R4,1(R4)                                                         
         STH   R4,MGACNT                                                        
*                                                                               
         LA    R1,MGERECL(R1)      BUMP TO NEXT ENTRY                           
         L     R4,ATABLEX          CHECK IF TABLE IS FULL                       
         CR    R1,R4                                                            
         BNL   TABFULER                                                         
         XC    0(MGERECL,R1),0(R1) CLEAR NEXT POSITION IN TABLE                 
         ST    R1,ANENTRY          SET A(NEXT ENTRY)                            
*                                                                               
         XC    MISSTOT,MISSTOT     CLEAR ACCUMULATORS                           
         XC    MGTOT,MGTOT                                                      
         XC    MISSRTG,MISSRTG                                                  
         XC    MGRTG,MGRTG                                                      
*                                                                               
TBX      B     XIT                                                              
         EJECT                                                                  
*        PUT A TOTAL RECORD                                                     
*                                                                               
PUTTOT   NTR1                                                                   
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         MVC   MGETMISS,MISSTOT                                                 
         MVC   MGETMG,MGTOT                                                     
         MVC   MGEMSRTG,MISSRTG                                                 
         MVC   MGEMGRTG,MGRTG                                                   
         MVI   BYTE,TSAPUT         PUT RECORD TO TSAR                           
         BAS   RE,CALLTSAR                                                      
         B     XIT                                                              
         SPACE 2                                                                
*        CLEAR THE ACCUMULATORS                                                 
*                                                                               
CLRACCUM NTR1                                                                   
         XC    MISSTOT,MISSTOT     CLEAR ACCUMULATORS                           
         XC    MGTOT,MGTOT                                                      
         XC    MISSRTG,MISSRTG                                                  
         XC    MGRTG,MGRTG                                                      
         B     XIT                                                              
         EJECT                                                                  
*        BUILD THE TABLE OF ENTRIES                                             
*                                                                               
BLDTABLE NTR1                                                                   
         OC    MGATAB,MGATAB       IS AN AREA PASSED TO US                      
         BZ    BT10                                                             
         L     R4,MGATAB           A(TABLE)                                     
         OC    0(MGERECL,R4),0(R4) FIRST TIME IN - TABLE EMPTY                  
         BNZ   *+8                                                              
         ST    R4,ANENTRY          A(NEXT ENTRY)                                
         L     R4,ANENTRY                                                       
         L     R1,ATABLEX                                                       
         CR    R4,R1                                                            
         BNL   TABFULER                                                         
*                                                                               
         MVC   0(MGERECL,R4),ENTRY PUT ENTRY IN TABLE                           
         LA    R4,MGERECL(R4)      BUMP TO NEXT POSITION                        
         XC    0(MGERECL,R4),0(R4)                                              
         ST    R4,ANENTRY          SET A(NEXT ENTRY)                            
         B     BT20                                                             
*                                                                               
BT10     XC    TSARBLK,TSARBLK                                                  
         MVI   BYTE,TSAADD         ADD RECORD TO TSAR                           
         BAS   RE,CALLTSAR                                                      
*                                                                               
BT20     OC    MGAHOOK,MGAHOOK                                                  
         BZ    BTX                                                              
         L     RF,MGAHOOK          HOOK TO USER                                 
         L     RE,USERRD                                                        
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF               MAY RETURN CC                                
*                                                                               
BTX      B     XIT                                                              
*                                                                               
TABFULER MVI   MGAERR,MGAQTFUL                                                  
         B     BTX                                                              
         EJECT                                                                  
*        GET THE RATING                                                         
*                                                                               
GETRTG   NTR1                                                                   
         XC    RATING,RATING                                                    
         XC    RATINGB,RATINGB                                                  
         MVI   MGAERR,MGAQDMIS     A(DEMOS) MISSING                             
         OC    MGABRDEM,MGABRDEM   NEED A(SVBRDEM)                              
         BZ    GRX                                                              
         OC    MGADEM,MGADEM       AND A(SVDEMS)                                
         BZ    GRX                                                              
         MVI   MGAERR,0                                                         
*                                                                               
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         L     R3,MGAIO                                                         
         LA    R3,24(R3)                                                        
         BAS   RE,NEXTEL                                                        
         BNE   GRX                                                              
*                                                                               
         L     R4,MGABRDEM                                                      
         OC    0(118,R4),0(R4)                                                  
         BNZ   *+8                                                              
         L     R4,MGADEM                                                        
*                                                                               
GR10     CLI   1(R4),0             TEST E-O-L                                   
         BE    GRX                                                              
         XC    RATING,RATING                                                    
         XC    RATINGB,RATINGB                                                  
*                                                                               
         ZIC   R0,1(R3)             FIND DEMO IN BUYREC                         
         SH    R0,=H'24'                                                        
         BNP   GRX                                                              
         SRL   R0,3                                                             
         LA    R5,24(R3)                                                        
*                                                                               
GR20     CLC   0(3,R4),0(R5)                                                    
         BE    GR30                                                             
         LA    R5,8(R5)                                                         
         BCT   R0,GR20                                                          
         LA    R4,3(R4)                                                         
         B     GR10                                                             
*                                                                               
GR30     L     R0,4(R5)                                                         
         N     R0,=X'7FFFFFFF'                                                  
         BAS   RE,EDITDEM                                                       
         TM    4(R5),X'80'                                                      
         BZ    *+8                                                              
         MVI   RATING+7,C'*'                                                    
*                                                                               
GRX      B     XIT                                                              
         EJECT                                                                  
* SUBROUTINE TO FORMAT DEMO AND HUT VALUES                                      
*                                                                               
EDITDEM  DS    0H                                                               
         ZIC   R1,3(R5)             GET HUT VALUE                               
         MR    R0,R0                                                            
         AH    R1,=H'50'                                                        
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         STCM  R0,15,RATINGB                                                    
         TM    MGAOPT,MGONEDIT                                                  
         BNO   ED10                                                             
         STCM  R0,15,RATING                                                     
         BR    RE                                                               
*                                                                               
ED10     EDIT  (R0),(4,RATING),1                                                
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        GET NEXT AVAILABLE CODE                                                
*                                                                               
GETCODE  NTR1                                                                   
         LA    R4,ENTRY                                                         
         USING MGENTRYD,R4                                                      
         XC    ENTRY,ENTRY                                                      
         MVI   TSERRS,0                                                         
         SR    R2,R2               SET COUNTER                                  
*                                                                               
         MVI   BYTE,TSARDH         READ HI                                      
         B     GC12                                                             
*                                                                               
GC10     MVI   BYTE,TSANXT                                                      
*                                                                               
GC12     BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BZ    GC14                                                             
         LA    R2,1(R2)            USE NEXT NUMBER                              
         B     GC20                                                             
*                                                                               
GC14     CLI   MGETYPE,1           TEST MAKEGOOD                                
         BNE   GC10                NO - SKIP                                    
         CLM   R2,1,MGAECOD        TEST THIS NUMBER MATCHES                     
         BE    GC10                YES - CONTINUE                               
         LA    R2,1(R2)            TRY NEXT NUMBER                              
         CLM   R2,1,MGAECOD        IF DIFFERENT, IT'S AVAILABLE                 
         BE    GC10                ELSE CONTINUE                                
*                                                                               
GC20     CH    R2,=X'003C'         NEXT AVAILABLE CODE                          
         BNL   GCERR                                                            
         STC   R2,BCODE            SET UP TO TRANSLATE                          
         MVI   BYTE,0                                                           
         BAS   RE,TRANSCD          TRANSLATE CODE X'01-X'3C'TO A0-F9            
         MVC   MGECODE,CODE                                                     
         MVC   MGAECOD,BCODE                                                    
         MVC   MGAENTRY,ENTRY                                                   
         B     XIT                                                              
*                                                                               
GCERR    MVI   MGAERR,MGAQFULL     NO MORE CODES AVAILABLE                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        ADD AN ENTRY TO TSAR                                                   
*                                                                               
ADDENTRY NTR1                                                                   
         MVC   ENTRY,MGAENTRY      SET UP ENTRY PASSED BY USER                  
         MVI   BYTE,TSAADD         ADD RECORD TO TSAR                           
         BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSEDUP       DUPLICATE KEY                                
         BNO   *+8                                                              
         MVI   MGAERR,MGAQDUP                                                   
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BNO   *+8                                                              
         MVI   MGAERR,MGAQEOF                                                   
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        FIND AN ENTRY - KEY IN MGAENTRY                                        
*                                                                               
FNDENTRY NTR1                                                                   
         MVC   ENTRY,MGAENTRY                                                   
         XC    TSRNUM,TSRNUM                                                    
         MVI   BYTE,TSARDH         READ HI                                      
         BAS   RE,CALLTSAR                                                      
         CLI   TSERRS,0            IS THERE AN ERROR                            
         BE    *+8                                                              
         MVI   MGAERR,MGAQNF                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        DELETE AN ENTRY FROM TSAR                                              
*                                                                               
DELENTRY NTR1                                                                   
         MVC   ENTRY,MGAENTRY      SET UP ENTRY PASSED BY USER                  
         MVI   BYTE,TSADEL         DELETE A RECORD FROM TSAR                    
         BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSERNF       RECORD NOT FOUND                             
         BO    DE10                                                             
         LH    R1,MGACNT           DECREMENT NUMBER OF ENTRIES IN TABLE         
         BCTR  R1,0                                                             
         STH   R1,MGACNT                                                        
         B     *+8                                                              
*                                                                               
DE10     MVI   MGAERR,MGAQNF                                                    
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        FIND NEXT ENTRY - TSRNUM IN MGATSNUM                                   
*                                                                               
NXTENTRY NTR1                                                                   
         MVC   TSRNUM,MGATSNUM                                                  
         MVI   BYTE,TSANXT         READ NEXT                                    
         BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BE    *+8                                                              
         MVI   MGAERR,MGAQEOF                                                   
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        GET AN ENTRY - TSRNUM IN MGATSNUM                                      
*                                                                               
GETENTRY NTR1                                                                   
         MVC   TSRNUM,MGATSNUM                                                  
         MVI   BYTE,TSAGET         READ NEXT                                    
         XC    MGAENTRY,MGAENTRY                                                
         BAS   RE,CALLTSAR                                                      
         CLI   TSERRS,0                                                         
         BE    GEX                                                              
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BNE   GE10                                                             
         MVI   MGAERR,MGAQEOF                                                   
         B     GEX                                                              
*                                                                               
GE10     TM    TSERRS,TSERNF       RECORD NOT FOUND                             
         BNO   GEX                                                              
         MVI   MGAERR,MGAQNF                                                    
*                                                                               
GEX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET UP A PRINT LINE                                                    
*                                                                               
SETPRNT  NTR1                                                                   
         USING MGLINED,R2                                                       
         L     R2,MGALINE                                                       
         USING MGENTRYD,R4                                                      
         LA    R4,MGAENTRY                                                      
         MVC   MGLCODE,MGECODE                                                  
         CLI   MGETYPE,X'FE'       TOTAL ENTRY                                  
         BNE   PR10                                                             
         BAS   RE,PRTTOT                                                        
         B     PRX                                                              
*                                                                               
PR10     CLI   MGETYPE,0                                                        
         BNE   PR20                                                             
         MVC   MGLTYPE,=C'MS- '                                                 
****     MVC   MGLMSRTG,MGERTG       RATING                                     
         LA    R5,MGLMSRTG-1         MISSED RATING TOTAL                        
         B     PR30                                                             
*                                                                               
PR20     MVC   MGLTYPE,=C'MG+ '                                                 
***      MVC   MGLMGRTG,MGERTG       RATING                                     
         LA    R5,MGLMGRTG-1         MISSED RATING TOTAL                        
*                                                                               
PR30     ICM   R0,15,MGERTGB       BINARY RATING                                
         BAS   RE,PRTRTG                                                        
*                                                                               
         EDIT  MGELINE,(3,MGLLINE),FILL=0                                       
         CLC   MGEDATE,=X'FFFF'                                                 
         BNE   PR40                                                             
         MVC   MGLDATE,=C'*MISSED*'                                             
         B     PR80                                                             
*                                                                               
PR40     GOTO1 CDATCON,DMCB,(2,MGEDATE),(4,MGLDATE)                             
*                                                                               
PR50     CLI   MGESPNUM,1                                                       
         BE    PR60                                                             
         MVI   MGLDATE+5,C'-'                                                   
         LA    R5,MGLDATE+6                                                     
         EDIT  MGESPNUM,(2,(R5)),FILL=0                                         
*                                                                               
PR60     MVC   MGLDAYPT,MGEDAYPT                                                
         EDIT  MGESLN,(3,MGLSLN)                                                
         GOTO1 MGUNTIME,DMCB,MGETIME,MGLTIME,=C'N'                              
*                                                                               
         ICM   R0,15,MGECOST                                                    
         LA    R5,MGLMSCST                                                      
         CLI   MGETYPE,0                                                        
         BE    *+8                                                              
         LA    R5,MGLMGCST                                                      
*                                                                               
         TM    MGESTAT,MGESTAT_NTP TEST DF/NTP BUY                              
         BO    *+8                 YES - DO NOT SHOW COST                       
         BAS   RE,PRTAMT                                                        
*                                                                               
PR70     MVC   MGLPGMNM,MGEPGMNM     PROGRAM                                    
         OC    MGESTA,MGESTA                                                    
         BZ    PR80                                                             
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),MGESTA                                                 
         GOTO1 MGMSUNPK,DMCB,(X'80',WORK),WORK+10,WORK+15                       
         MVC   MGLSTA,WORK+15                                                   
*                                                                               
PR80     DS    0H                                                               
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINT OUT A TOTAL LINE                                                 
*                                                                               
PRTTOT   NTR1                                                                   
         USING MGLINED,R2                                                       
         USING MGENTRYD,R4                                                      
         CLC   =X'FEFE',MGECODE    GRAND TOTAL                                  
         BNE   *+10                                                             
         MVC   MGLCODE-1(4),=C'GRND'                                            
         MVC   MGLTYPE,=C'TOTL'                                                 
*                                                                               
         L     R0,MGETMISS         MISSED TOTAL                                 
         LA    R5,MGLMSCST                                                      
         BAS   RE,PRTAMT                                                        
*                                                                               
*****    MVC   MGLMSRTG,MGEMSRTE   RATING                                       
***      CLC   =X'FEFE',MGECODE    GRAND TOTAL                                  
***      BNE   PT10                                                             
         LA    R5,MGLMSRTG-1       MISSED RATING TOTAL                          
         L     R0,MGEMSRTG                                                      
         BAS   RE,PRTRTG                                                        
*                                                                               
PT10     LA    R5,MGLMGCST        MAKEGOOD TOTAL                                
         L     R0,MGETMG                                                        
         BAS   RE,PRTAMT                                                        
*                                                                               
***      MVC   MGLMGRTG,MGEMGRTE   RATING                                       
***      CLC   =X'FEFE',MGECODE    GRAND TOTAL                                  
***      BNE   PT20                                                             
         LA    R5,MGLMGRTG-1       MAKEGOOD RATING TOAL                         
         L     R0,MGEMGRTG                                                      
         BAS   RE,PRTRTG                                                        
*                                                                               
PT20     B     XIT                                                              
*                                                                               
*        EDIT OUT TO TOTAL LINE                                                 
*                                                                               
PRTAMT   NTR1                                                                   
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(8,0(R5)),FLOAT=$,ZERO=NOBLANK                              
         B     XIT                                                              
*                                                                               
*        EDIT OUT RATINGS TO TOTAL LINE                                         
*                                                                               
PRTRTG   NTR1                                                                   
**********((T  (R0),(5,0(R5)),1                                                 
         EDIT  (R0),(8,0(R5)),1                                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        READ HI - MGAENTRY SET                                                 
*                                                                               
RDHI     NTR1                                                                   
         XC    TSRNUM,TSRNUM                                                    
         MVC   ENTRY,MGAENTRY                                                   
         MVI   BYTE,TSARDH         READ HIGH                                    
         BAS   RE,CALLTSAR                                                      
         TM    TSERRS,TSEEOF       END OF FILE                                  
         BE    *+8                                                              
         MVI   MGAERR,MGAQEOF                                                   
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        CALL TSAR                                                              
*                                                                               
CALLTSAR NTR1                                                                   
         LA    R8,TSARBLK                                                       
         USING TSARD,R8                                                         
         MVC   TSACOM,MGAACOM      A(COMFACS)                                   
         MVI   TSPAGL,1            USE TEMPSTR PAGE 1                           
         MVI   TSPAGN,2            USE 2 PAGES  <=== SEE T21130                 
         MVI   TSKEYL,MGEKEYL      KEY LENGTH                                   
         LA    R1,MGERECL          RECORD LENGTH                                
         STH   R1,TSRECL                                                        
         OI    TSINDS,TSIXTTWA     14K RECORDS                                  
         MVC   TSACTN,BYTE                                                      
         LA    R1,ENTRY            A(RECORD)                                    
         ST    R1,TSAREC                                                        
         GOTO1 MGATSAR,TSARBLK                                                  
         MVC   MGATSNUM,TSRNUM     RETURN TSAR RECORD NUMBER                    
         MVC   MGAENTRY,ENTRY                                                   
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        DATAMGR ROUTINES                                                       
*                                                                               
HIGH     NTR1                                                                   
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIR                                                              
*                                                                               
SEQ      NTR1                                                                   
         MVC   COMMAND,=C'DMRSEQ'                                               
*                                                                               
DIR      GOTO1 CDATAMGR,DMCB,COMMAND,=C'SPTDIR',KEY,KEY                         
         B     DIRX                                                             
*                                                                               
GETREC   NTR1                                                                   
         MVC   COMMAND,=C'GETREC'                                               
         GOTO1 CDATAMGR,DMCB,=C'GETREC',=C'SPTFILE',KEY+14,MGAIO,DMWORK         
*                                                                               
DIRX     CLI   DMCB+8,0            TEST DATAMGR ERROR                           
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
MGAXIT   XMOD1 1                                                                
         EJECT                                                                  
NEXTEL   CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
NEXTEL2  CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCDLO,0(R3)                                                     
         BH    NEXTEL                                                           
         CLC   ELCDHI,0(R3)                                                     
         BL    NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
USERRD   DS    A                                                                
ANENTRY  DS    A                                                                
DUB      DS    D                                                                
DMWORK   DS    12D                                                              
TSARBLK  DS    CL48                                                             
DMCB     DS    6F                                                               
SAVER3   DS    F                                                                
MISSTOT  DS    F                   TOTAL ACCUMULATORS                           
MGTOT    DS    F                                                                
GMISSTOT DS    F                                                                
GMGTOT   DS    F                                                                
MISSRTG  DS    F                                                                
MGRTG    DS    F                                                                
GMISSRTG DS    F                                                                
GMGRTG   DS    F                                                                
*                                                                               
BUYCOST  DS    F                                                                
LASTCOST DS    F                                                                
ATABLEX  DS    F                                                                
DATADISP DS    H                                                                
HALF     DS    H                                                                
BYTE     DS    XL1                                                              
BYTE2    DS    XL1                                                              
BYTE3    DS    XL1                                                              
BYTE4    DS    XL1                                                              
SVELCDS  DS    XL2                                                              
KEY      DS    CL20                                                             
KEYSAVE  DS    CL20                                                             
WORK     DS    CL48                                                             
RATING   DS    CL10                                                             
RATINGB  DS    XL4                                                              
ELCODE   DS    XL1                                                              
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
BUYCIND  DS    XL1                 X'01'=MINUS                                  
BUYSTAT  DS    XL1                 SEE MGESTAT                                  
COMMAND  DS    CL8                                                              
ENTRY    DS    CL(MGERECL)                                                      
ENTRY2   DS    CL(MGERECL)                                                      
TOTFND   DS    XL1                                                              
SPOTNUM  DS    XL1                                                              
LASTDATE DS    XL2                                                              
THISCODE DS    CL2                                                              
THISSTA  DS    XL3                                                              
CODE     DS    CL2                                                              
BCODE    DS    XL1                                                              
PRD1     DS    XL1                                                              
SLN1     DS    XL1                                                              
PRD2     DS    XL1                                                              
SLN2     DS    XL1                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPMGAD2                                                        
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
*                                                                               
LINED    DSECT                                                                  
         DS    CL1                                                              
LCODE    DS    CL2                                                              
         DS    CL4                                                              
LTYPE    DS    CL3                                                              
         DS    CL4                                                              
LLINE    DS    CL3                                                              
         DS    CL4                                                              
LDATE    DS    CL8                                                              
         DS    CL3                                                              
LSLN     DS    CL3                                                              
         DS    CL3                                                              
LTIME    DS    CL11                                                             
         DS    CL2                                                              
LMSCOST  DS    CL8                                                              
         DS    CL4                                                              
LMSRTG   DS    CL4                                                              
         DS    CL4                                                              
LMGCOST  DS    CL8                                                              
         DS    CL4                                                              
LMGRTG   DS    CL4                                                              
         DS    CL3                                                              
LSTA     DS    CL8                                                              
         DS    CL3                                                              
LPGMNM   DS    CL14                                                             
         EJECT                                                                  
TLINED   DSECT                                                                  
LTMISS   DS    CL8                                                              
         DS    CL1                                                              
LTMSRTG  DS    CL5                                                              
         DS    CL1                                                              
LTMG     DS    CL8                                                              
         DS    CL1                                                              
LTMGRTG  DS    CL5                                                              
         EJECT                                                                  
*SPGENBUY                                                                       
*DDCOMFACSD                                                                     
*DDCOREQUS                                                                      
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034ANBLDXX   05/01/02'                                      
         END                                                                    

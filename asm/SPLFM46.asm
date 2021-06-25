*          DATA SET SPLFM46    AT LEVEL 073 AS OF 05/01/02                      
*PHASE T21946A                                                                  
         TITLE 'SPLFM46 - PRODUCT EXCLUSION RECORDS'                            
T21946   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21946                                                         
         L     RC,0(R1)                                                         
         LA    R6,1(RC)                                                         
         LA    R6,4095(R6)                                                      
         USING GENOLD,RC,R6                                                     
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING PXCRECD,R8                                                       
         USING PXCEL05,R3                                                       
*                                                                               
         CLI   SVFMTSW,0           IS IT A FORMAT                               
         BNE   EDT10               NO.                                          
         SPACE                                                                  
* THIS SECTION DISPLAYS THE RECORD *                                            
         SPACE                                                                  
FMT00    XC    KEY,KEY                                                          
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
FMT05    LA    R3,REC+24          R3 POINTS TO 01 ELEMENT                       
         MVI   ELCODE,5                                                         
         BAS   RE,NEXTEL          R3 POINTS TO 05 ELEMENT                       
         BE    *+8                                                              
         B     EXIT                                                             
* CLEARS THE SCREEN                                                             
         LA    R2,PXCF1H                                                        
FMT10    CLI   1(R2),X'20'         IS IT A PROTECTED FIELD                      
         BE    FMT20                                                            
         ZIC   RF,0(R2)            RF HAS LENGTH OF FIELD                       
         SH    RF,=H'9'            SUBTRACT HEADER LENGTH +1                    
         EX    RF,CLROC            IS FIELD EMPTY                               
         EX    RF,CLRCLC            "   "     "                                 
         BE    FMT20               YES.                                         
         EX    RF,CLRXC            NO. XC FIELD.                                
         FOUT  (R2)                                                             
FMT20    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    FMT10                                                            
         B     FMT25                                                            
*                                                                               
CLROC    OC    8(0,R2),SPACES                                                   
CLRCLC   CLC   8(0,R2),SPACES                                                   
CLRXC    XC    8(0,R2),8(R2)                                                    
         SPACE                                                                  
* GET THE 05 ELEMENTS                                                           
FMT25    DS    0H                                                               
         LA    R2,PXCF1H                                                        
         B     FMT40                                                            
FMT30    BAS   RE,NEXTEL                                                        
         BNE   EXIT                     NO MORE 05 ELEMENTS                     
FMT40    CLI   PXCPGM,0            TIME CODE                                    
         BE    FMT60                                                            
         CLI   PXCPGM,1            DAY CODE                                     
         BE    FMT70                                                            
         CLI   PXCPGM,2            TIME + DAY CODE                              
         BE    FMT80                                                            
         MVC   8(L'PXCPGM,R2),PXCPGM                                            
FMT40A   FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 VDATCON,DMCB,(2,PXCSTDTE),(5,8(R2))                              
         CLC   PXCSTDTE,PXCEDTE         IF START/END DATES ARE THE SAME         
         BE    FMT50                    DISPLAY ONLY ONE.                       
         GOTO1 (RF),(R1),(2,PXCEDTE),(5,17(R2))                                 
         MVI   16(R2),C'-'                                                      
FMT50    FOUT  (R2)                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    FMT30                                                            
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
FMT60    DS    0H                                                               
         MVC   DMCB+4(4),=X'D9000A11'     * UNTIME                              
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+1,10(R2)                                        
         MVC   8(2,R2),=C'T/'                                                   
         B     FMT40A                                                           
         SPACE                                                                  
*                                                                               
FMT70    DS    0H                                                               
         MVC   DMCB+4(4),=X'D9000A0F'     * DAYUNPK                             
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+1,10(R2)                                        
         MVC   8(2,R2),=C'D/'                                                   
         B     FMT40A                                                           
         SPACE                                                                  
FMT80    DS    0H                  TIME + DAY                                   
         MVI   WORK,C' '                                                        
         MVC   WORK+1(25),WORK                                                  
         MVC   DMCB+4(4),=X'D9000A11'     * UNTIME                              
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+1,WORK                                          
         LA    R4,WORK+15          POINT WELL PAST END OF TIME                  
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'/'                                                       
         MVC   DMCB+4(4),=X'D9000A0F'     * DAYUNPK                             
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,PXCPGM+5,2(R4)   DAY IS AFTER TIME                     
*                                                                               
         MVC   8(2,R2),=C'T/'                                                   
         MVC   10(17-2,R2),WORK    SCRREN FIELD LENGTH -2                       
         B     FMT40A                                                           
         EJECT                                                                  
* THIS SECTION ADDS A RECORD OR CHANGES THE 05 ELEMENTS *                       
         SPACE                                                                  
EDT10    LA    R3,REC+24           R3 POINTS TO 01 ELEMENT                      
         CLI   SVACT,C'A'          IS IT AN ADD                                 
         BE    EDT20                                                            
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         LA    R2,PXCF1            IS IT A DELETE                               
         CLC   0(6,R2),=C'DELETE'                                               
         BE    DELRTN                                                           
         SPACE                                                                  
* DELETE THE 05 ELEMENTS                                                        
         MVI   ELCODE,5                                                         
         BAS   RE,NEXTEL                                                        
         BNE   EDT20A              NO 05 ELEMENTS                               
EDT15    GOTO1 VRECUP,DMCB,(0,REC),0(R3),0                                      
         CLI   0(R3),5             IS IT AN 05 ELEMENT                          
         BE    EDT15               YES. DELETE IT.                              
         B     EDT20A              NO MORE 05 ELEMENTS.                         
         SPACE                                                                  
* BUILD THE NEW KEY                                                             
EDT20    LA    RE,REC                                                           
         LA    RF,L'REC                                                         
         XCEF                                                                   
         MVC   REC(13),SVKEY                                                    
         MVI   REC+14,29           LENGTH OF PXCREC WITHOUT 05 ELEM             
         MVC   PXCAGYA,AGYALPHA                                                 
         SPACE                                                                  
* BUILD THE 01 ELEMENT                                                          
         MVC   0(2,R3),=X'0105'                                                 
EDT20A   GOTO1 VDATCON,DMCB,(5,0),(3,PXCACDAT)       NEW ACT DATE               
         SPACE                                                                  
* BUILD THE 05 ELEMENT                                                          
         LA    R2,PXCF1H                                                        
EDT30    LA    R3,REC+24                                                        
         CLI   5(R2),0             TEST IF PRGM FIELD IS EMPTY                  
         BE    EDT40                                                            
         MVC   ELEM(2),=X'0517'                                                 
         MVC   ELEM+2(L'PXCPGM),8(R2)                                           
         SPACE 2                                                                
         CLC   ELEM+2(2),=C'P/'        TEST NTWK CODE                           
         BNE   EDT31                                                            
         MVC   DEMAREA+3(6),10(R2)     SAVE NTWK PROGRAM CODE                   
         OC    DEMAREA+3(6),SPACES                                              
         MVI   DEMAREA+1,C'Y'                                                   
         LR    R5,R2                   SAVE HEADER ADDRESS                      
         B     EDT30AA                                                          
         SPACE                                                                  
EDT31    CLC   ELEM+2(2),=C'T/'    TIME EXPRESSION                              
         BE    TIMED                                                            
         CLC   ELEM+2(2),=C'D/'    DAY EXPRESSION                               
         BE    DAYED                                                            
EDT30AA  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             IS THERE A START/END DATE                    
         BNE   EDT30A              YES                                          
         MVI   ERRCD,MSSNGERR      NO. ERROR.                                   
         B     ERRORTN                                                          
EDT30A   MVI   ERRCD,DATERR                                                     
         LA    R7,8(R2)                                                         
         GOTO1 VDATVAL,DMCB,(0,(R7)),WORK                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRORTN                                                          
         A     R7,0(R1)                                                         
         CLI   0(R7),C'-'                                                       
         BE    EDT30B                                                           
         CLI   0(R7),C','                                                       
         BE    EDT30B                                                           
         MVC   WORK+6(6),WORK                                                   
         B     EDT30C                                                           
EDT30B   LA    R7,1(R7)                                                         
         GOTO1 VDATVAL,DMCB,(0,(R7)),WORK+6                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRORTN                                                          
EDT30C   GOTO1 VDATCON,DMCB,(0,WORK),(2,ELEM+19)                                
         GOTO1 (RF),(R1),(0,WORK+6),(2,ELEM+21)                                 
         CLC   ELEM+19(2),ELEM+21                                               
         BC    12,EDT30CC                   ST DTE EQU/LESS END DTE             
         MVI   ERRCD,SPDERR                                                     
         B     ERRORTN                                                          
         EJECT                                                                  
*                                                                               
EDT30CC  MVI   ELCODE,5           * ROUTINE TO ADD ELEMS IN A/N ORDER           
         SPACE                                                                  
         CLI   DEMAREA+1,C'Y'      TEST NTWK CODE                               
         BNE   EDT30D                                                           
         BAS   RE,NTWKRTN          YES.GOTO NETWORK ROUTINE                     
         SPACE                                                                  
         CLI   ERRAREA,X'FF'       WAS ERROR SET IN NTWKRTN                     
         BNE   EDT30D                                                           
         OI    6(R5),X'40'         YES/ POSITION CURSOR                         
         OI    6(R5),X'80'              TRANSMIT BIT                            
         B     EXIT                                                             
         SPACE                                                                  
EDT30D   BAS   RE,NEXTEL                                                        
         BNE   EDT30E                                                           
         CLC   ELEM(23),0(R3)                                                   
         BH    EDT30D                                                           
         BE    DUPERRTN                                                         
EDT30E   GOTO1 VRECUP,DMCB,(0,REC),ELEM,0(R3)                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BE    WRITE10             END OF SCREEN                                
         B     EDT30                                                            
*                                                                               
EDT40    LR    R7,R2               LOAD PGM FIELD INTO R7                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             IS START/END DATE ALSO BLANK                 
         BE    EDT50                                                            
         MVI   ERRCD,MSSNGERR      NO. ERROR.                                   
         LR    R2,R7               LOAD PGM FIELD INTO R2                       
         B     ERRORTN                                                          
EDT50    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BE    WRITE10             END OF SCREEN                                
         B     EDT30                                                            
         EJECT                                                                  
* WRITE THE RECORD *                                                            
         SPACE                                                                  
WRITE10  CLI   SVACT,C'A'                                                       
         BE    WRITE20                                                          
         BAS   RE,ANYCHK                                                        
         SPACE                                                                  
         LA    R8,REC2             REREAD RECORD  INTO REC2 TO SET              
         ST    R8,AREC             DMWORK FOR PUTREC                            
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         SPACE                                                                  
         LA    R8,REC              RESTORE AREC WITH REC                        
         ST    R8,AREC                                                          
         GOTO1 PUTREC                                                           
         B     EXIT                                                             
WRITE20  BAS   RE,ANYCHK                                                        
         GOTO1 ADDREC                                                           
         B     EXIT                                                             
         EJECT                                                                  
*** ROUTINES REFERRED TO IN PROGRAM ***                                         
*                                                                               
* NETWORK ROUTINE TO GET NTWK PRG NAME *                                        
         SPACE                                                                  
NTWKRTN  DS    0H                                                               
         NTR1                                                                   
         LA    R3,REC2+10          USE REC2 IN THIS ROUTINE                     
         ST    R3,AREC                                                          
         CLI   DEMAREA,1           TEST FIRST TIME                              
         BE    NT10                                                             
         MVI   DEMAREA,1                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'            SET UP TO READ STA MASTER REC                
         MVC   KEY+1(1),SVEBCMED                                                
         MVC   KEY+2(5),SVSTA                                                   
         MVC   KEY+7(2),AGYALPHA                                                
         MVC   KEY+9(3),SVEBCCLT                                                
         MVC   KEY+12(5),=5C'0'                                                 
         SPACE                                                                  
         GOTO1 RDSTA                                                            
         SPACE                                                                  
         USING STAREC,R3                                                        
         PACK  DUB,SMKT                                                         
         CVB   R1,DUB                                                           
         STCM  R1,3,SVMKT                                                       
         SPACE                                                                  
NT10     XC    KEY,KEY             SET UP TO READ NTWK REC                      
         MVC   KEY(2),=X'0D20'                                                  
         MVC   KEY+2(1),SVKEY+2    AGY/MED                                      
         MVC   KEY+3(2),SVMKT      MARKET                                       
         MVC   KEY+5(6),DEMAREA+3  NTWK PRG CODE SAVED IN DEMAREA+3             
         MVC   KEY+11(2),ELEM+19   START DATE(COMPRESSED FROM SCREEN)           
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   NONTWK                                                           
         SPACE                                                                  
         GOTO1 GETREC                                                           
         USING NPGELEM,R3                                                       
         LA    R3,24(R3)                R3 - 01 ELEM                            
         MVI   ELCODE,X'92'                                                     
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM+2(16),NPGNAME                                               
         MVI   ELEM+18,C' '             PXCPGM=17 CHARS                         
         XC    8(17,R5),8(R5)           HEADER SAVED IN R5                      
         MVC   8(16,R5),NPGNAME                                                 
         OI    6(R5),X'80'              TRANSMIT BIT                            
         SPACE                                                                  
         MVC   KEY,SVKEY           RESTOR KEY/AREC                              
         ST    R8,AREC                                                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
NONTWK   DS    0H                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(32),=C'** NTWK PROGRAM REC NOT FOUND **'                  
         FOUT  LFMMSGH                                                          
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
NEXTEL   CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R3)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         CLC   ELCODE,0(R3)                                                     
         BNE   NEXTEL                                                           
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  EXIT WITH CC NOT EQUAL                       
*                                                                               
DELRTN   MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),SVKEY                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,SVKEY                                                        
         MVI   KEY+13,X'80'                                                     
         MVC   COMMAND,=CL8'DMWRT'                                              
         GOTO1 DIR                                                              
         MVI   REC+15,X'80'                                                     
         GOTO1 PUTREC                                                           
         XC    LFMMSG,LFMMSG                                                    
         FOUT  LFMMSGH,=C'RECORD DELETED',14                                    
         MVI   ERRAREA,1                                                        
         B     EXIT                                                             
         SPACE                                                                  
*                                                                               
TIMED    DS    0H                                                               
         XC    ELEM+2(21),ELEM+2   CLEAR ELEMENT                                
*                                  FIND LENGTH FOR TIME EDIT                    
         LA    R4,10(R2)           BYPASS T/                                    
*                                                                               
TM2      DS    0H                                                               
         CLI   0(R4),C'/'          END WITH /                                   
         BE    TM3                                                              
         CLI   0(R4),C' '          OR BLANK                                     
         BNH   TM3                                                              
         LA    R4,1(R4)                                                         
         B     TM2                                                              
*                                                                               
TM3      DS    0H                                                               
         LA    R0,10(R2)                                                        
         SR    R4,R0               R4 HAS LENGTH TIME PORTION                   
         XC    WORK2(4),WORK2                                                   
         MVC   DMCB+4(4),=X'D9000A0E'        *TIMVAL                            
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,((R4),10(R2)),WORK2                                    
         CLI   0(R1),X'FF'                                                      
         BNE   TM10                                                             
         MVI   ERRCD,INVERR                                                     
         B     ERRORTN                                                          
TM10     MVC   ELEM+3(4),WORK2     SET TIME                                     
         MVI   ELEM+2,0                                                         
*                                                                               
         LA    RF,8+16(R2)         END OF FIELD                                 
*                                  GET LENGTH OF POSSIBLE DAY PORTION           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    R0,10(R2,R4)        POINT 1 PAST TIME PART                       
         SR    RF,R0                                                            
         BNP   TM15                NO FURTHER INPUT                             
*                                                                               
         LA    R4,10+1(R2,R4)                                                   
         ST    R4,FULL             START OF DAY PART                            
         STC   RF,FULL                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A03'        *DAYPAK                            
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         MVC   DMCB,FULL           ADDR OF DAY                                  
         GOTO1 (RF),DMCB,,WORK2,WORK2+16                                        
         CLI   WORK2,0                                                          
         BNE   TM14                                                             
         MVI   ERRCD,INVERR                                                     
         B     ERRORTN                                                          
TM14     MVI   ELEM+2,2            TIME + DAY                                   
         MVC   ELEM+7(1),WORK2     DAY AFTER TIME                               
*                                                                               
TM15     DS    0H                                                               
         B     EDT30AA                                                          
         SPACE                                                                  
*                                                                               
DAYED    DS    0H                                                               
         XC    ELEM+2(21),ELEM+2                                                
         ZIC   R4,5(R2)                      L'INPUT TO R4                      
         SH    R4,=H'2'                      MINUS 2 (FOR 'D/')                 
         MVC   DMCB+4(4),=X'D9000A03'        *DAYPAK                            
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,((R4),10(R2)),WORK2,WORK2+16                           
         CLI   WORK2,0                                                          
         BNE   DY10                                                             
         MVI   ERRCD,INVERR                                                     
         B     ERRORTN                                                          
DY10     MVI   ELEM+2,1            DAY CODE                                     
         MVC   ELEM+3(1),WORK2                                                  
         B     EDT30AA                                                          
         SPACE                                                                  
*                                                                               
ANYCHK   CLC   REC+13(2),=X'0029'  WAS SCREEN LEFT BLANK ON CHA/ADD             
         BHR   RE                                                               
         LA    R2,PXCF1H           YES. ERROR.                                  
         MVI   ERRCD,MSSNGERR                                                   
         B     ERRORTN                                                          
*                                                                               
DUPERRTN DS    0H                                                               
         ZIC   R0,0(R2)            MOVE CURSOR BACK TO PGM FIELD                
         SR    R2,R0                                                            
         MVI   ERRCD,DUPENTRY                                                   
         B     ERRORTN                                                          
*                                                                               
ERRORTN  GOTO1 ERROR                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         EJECT                                                                  
         ORG   LFMTABH                                                          
       ++INCLUDE SPLFMC6D                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPXC                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073SPLFM46   05/01/02'                                      
         END                                                                    

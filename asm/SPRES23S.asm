*          DATA SET SPRES23S   AT LEVEL 027 AS OF 05/01/02                      
*PHASE T20F23,*                                                                 
*INCLUDE BINSRCH2                                                               
         TITLE 'T20F23- RESEARCH STATION/BOOK LIST'                             
T20F23   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20F23,RR=R2                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R3,ATWA                                                          
         USING CONHEADH-64,R3                                                   
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     RF,=V(BINSRCH)                                                   
         AR    RF,R2               RELOCATE BINSRCH                             
         ST    RF,BINSRCH                                                       
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB               SET CC EQ                                    
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQ                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
VK       DS    0H                                                               
*                                                                               
         LA    R2,STAMEDH                                                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
*                                                                               
         CLI   8(R2),C'T'          TEST TELE                                    
         BE    VK10                                                             
         CLI   8(R2),C'R'          TEST RADIO                                   
         BNE   VK05                                                             
*                                                                               
         MVI   RADMED,C'Y'                                                      
         B     VK10                                                             
*                                                                               
VK05     MVI   ERROR,INVALID                                                    
         B     EDTERR                                                           
*                                                                               
VK10     LA    R2,STASRCH          VALIDATE SOURCE FIELD                        
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
*                                                                               
         GOTO1 VVALSRC                                                          
*                                                                               
         CLI   RADMED,C'Y'                                                      
         BNE   VK20                                                             
         MVI   DBSELMED,C'R'                                                    
*              NOTE:NO LONGER CORRECT RAD BAGYMED                               
*                                                                               
VK20     LA    R2,STASTAH          VALIDATE STATION FIELD                       
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
*                                                                               
         BAS   RE,VALSTA                                                        
*                                                                               
*                                                                               
VK60     DS    0H                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*              FORMAT ONLINE LIST SCREEN                                        
*                                                                               
*                                                                               
LR       OC    KEY(18),KEY         TEST FIRST TIME THROUGH                      
         BZ    LR10                OR CHAR INCL DISK ADD                        
         BAS   RE,GETSAVE                                                       
         LA    R1,BUFF                                                          
         ST    R1,BINATAB                                                       
         L     R1,SVTBLNDX                                                      
         ZIC   R0,LISTN                                                         
         AR    R1,R0               BUMP TABLE INDEX ON NEW SCREEN               
         ST    R1,SVTBLNDX                                                      
         C     R1,SVTBLSIZ         TEST END OF TABLE                            
         BH    LR100                                                            
         B     LR20                                                             
*                                                                               
LR10     XC    SVTBLNDX,SVTBLNDX                                                
         XC    SVTBLSIZ,SVTBLSIZ                                                
         BAS   RE,DEMPROC          BUILD THE LIST TABLE                         
         MVC   SVTBLSIZ,BINSOFAR    TABLE SIZE=#MARKETS                         
         MVI   LISTN,LISTNQ        SET #LIST LINES                              
         MVC   STAHDLN(L'LISTHD),LISTHD        MOVE IN HEADLINE                 
         OI    STAHDLNH+6,X'80'    TRANSMIT                                     
         BAS   RE,PUTSAVE                                                       
*                                                                               
LR20     L     R7,BINATAB                                                       
         USING BINRECD,R7                                                       
         L     R0,SVTBLNDX                                                      
         MH    R0,=Y(BINEND-BINRECD)                                            
         AR    R7,R0               R7=A(FIRST ENTRY TO DISPLAY)                 
*                                                                               
LR30     LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES                                                    
         USING LSTLINE,R2                                                       
*                                                                               
LR40     CLC   BINBOOK(2),=2X'FF'       TEST FOR END OF LIST                    
         BE    LR100                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,BINBOOK),(6,LSTBOOK)                              
*                                                                               
         CLI   ENDNEAR,C'Y'                                                     
         BE    LR50                NO NEED TO DO SECOND COLUMN                  
*                                                                               
LR45     ZIC   RE,LISTN                                                         
         LA    RE,1(RE)                                                         
         MH    RE,=Y(BINEND-BINRECD)     BUMP AHEAD 16 ENTRIES                  
         AR    R7,RE                                                            
*                                                                               
         CLC   BINBOOK(2),=2X'FF'        TEST FOR END OF LIST                   
         BNE   *+12                                                             
         MVI   ENDNEAR,C'Y'                                                     
         B     LR50                                                             
*                                                                               
         OC    BINBOOK,BINBOOK     SEE IF ANY DATA                              
         BZ    LR50                                                             
         GOTO1 DATCON,DMCB,(3,BINBOOK),(6,LSTBOOK2)                             
*                                                                               
LR50     L     R1,SVTBLNDX                                                      
         LA    R1,1(R1)                                                         
         ST    R1,SVTBLNDX         BUMP NEXT ENTRY                              
         MVC   KEY(13),=CL13'TABLE INDEX  '                                     
         XC    KEY+14(2),KEY+14    CLEAR DISC ADDRESS                           
         MVC   KEY+16(2),=2X'01'                                                
         MVC   DMDSKADD,KEY+14     SET DISK ADD FOR GENCON                      
*                                                                               
         DROP  R7                                                               
*                                                                               
LR80     GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR20                                                             
*                                                                               
LR100    XC    KEY,KEY                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                         PROCEDURE TO CALL DEMAND TO GET MKT RECORDS           
*                                                                               
DEMPROC  NTR1                                                                   
         LA    R2,STASRCH          SET FOR VALSRC CALL                          
         GOTO1 VVALSRC             RESET DBLOCK VALUES                          
*                                                                               
         CLI   RADMED,C'Y'         TEST RADIO                                   
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'       FUDGE DUE TO MEDIA FIELD                     
*                                                                               
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMB     GET BOOKS FOR A STATION                      
         MVC   DBSELSTA,ACTSTAT                                                 
*                                                                               
         LA    R0,POSTLINE         SET PARAMETERS FOR BINSRCH                   
         ST    R0,BINAREC                                                       
         LA    R0,BUFF             TABLE SET IN 12K BUFFER                      
         ST    R0,BINATAB                                                       
*                                                                               
         L     R1,=AL4(BUFFEND-BUFF)      SET MAX# REC IN BUFFER                
         SR    R0,R0                                                            
         LA    RE,BINEND-BINRECD                                                
         DR    R0,RE                                                            
         ST    R1,BINMAXN                                                       
*                                                                               
         XC    BINSOFAR,BINSOFAR                                                
         LA    R0,BINEND-BINRECD    LENGTH OF RECORD                            
         ST    R0,BINLREC                                                       
*                                                                               
DEM1     LA    R0,BINBOOK-BINRECD                                               
         LA    RF,L'BINBOOK                                                     
         ST    RF,BINLKEY                                                       
         STC   R0,BINDKEY          NUMERIC SORT                                 
*                                                                               
*                                  CALL DEMAND TO READ RECORDS                  
DEM2     GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK,0                                     
         LA    R2,POSTLINE                                                      
         USING BINRECD,R2                                                       
         MVC   BINBOOK(2),=2X'FF'         CREATE END OF LIST MARKER             
         BAS   RE,POST                                                          
         L     R1,BINSOFAR         ADD LAST LINE TO TAB COUNT                   
         LA    R1,1(R1)                                                         
         ST    R1,BINSOFAR                                                      
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND                         
*                                                                               
DEMHOOK  ST    RE,SAVERE           SAVE RETURN ADDRESS                          
*                                  SEARCH RECORD FOR NAME ELEMENT               
         L     R7,DBAREC                                                        
         USING SBKEY,R7           R1=A(FIRST ELEMENT)                           
*                                  BUILD BINSRCH RECORD & POST                  
DEMHOOK4 LA    R4,POSTLINE                                                      
         USING BINRECD,R4                                                       
         CLI   SBBOOK,99           BYPASS BAD STATION RECORDS                   
         BH    DEMHOOKX                                                         
         CLI   SBHOME,C'H'         TEST RADIO HOME MARKET                       
         BE    *+14                 ACCEPT IF IT IS                             
         OC    SBKMKT,SBKMKT       IGNORE SPILL MARKETS                         
         BNZ   DEMHOOKX                                                         
         MVC   BINBOOK,SBBOOK                                                   
         BAS   RE,POST                                                          
         DROP  R4                                                               
*                                  RETURN TO DEMAND                             
DEMHOOKX L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R7                                                               
         EJECT                                                                  
*              CODE TO POST LINE TO BUFFER TABLE VIA BINSRCH                    
*                                                                               
POST     NTR1                                                                   
         LA    R1,POSTLINE         SET A(BINSRCH RECORD)                        
         ST    R1,BINAREC                                                       
         MVI   BINCMND,1           INSERT ENTRY IN TABLE                        
*                                                                               
POST2    GOTO1 BINSRCH,BINPARMS    CALL BINSRCH                                 
         OC    BINAREC+1(3),BINAREC+1                                           
         BNZ   POSTX                                                            
*                                                                               
POST4    LA    R2,STASRCH                                                       
         MVI   ERROR,TABFUL        TABLE FULL - EXIT WITH ERROR                 
         B     EDTERR                                                           
*                                                                               
POSTX    B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
*                                                                               
*                                                                               
*ROUTINE TO WRITE TABLE TO SAVE STORAGE IN BETWEEN TRANSACTIONS                 
*                                                                               
PUTSAVE  NTR1                                                                   
         LA    R0,2                LOOP COUNTER                                 
         LA    R2,2                R2=TWA START PAGE NUMBER                     
         SR    R3,R3                                                            
         ICM   R3,3,TERM           R3=TERMINAL NUMBER                           
         LA    R4,BUFF             R4=I/O ADDRESS                               
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(4000)                                              
PUTSAVE2 GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',((R2),(R3)),(R4),,            
         LA    R4,4000(R4)         BUMP TO NEXT RECORD ADDRESS                  
         LA    R2,1(R2)            BUMP PAGE NUMBER                             
         BCT   R0,PUTSAVE2         DO FOR NUMBER OF TWA'S                       
         B     XIT                                                              
*                                                                               
*ROUTINE TO READ TABLE FROM SAVE STORAGE AFTER TRANSACTIONS                     
*                                                                               
*                                                                               
GETSAVE  NTR1                                                                   
         LA    R0,2                LOOP COUNTER                                 
         LA    R2,2                R2=TWA START PAGE NUMBER                     
         SR    R3,R3                                                            
         ICM   R3,3,TERM           R3=TERMINAL NUMBER                           
         LA    R4,BUFF             R4=I/O ADDRESS                               
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(4000)                                              
GETSAVE2 GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',((R2),(R3)),(R4),,           
         LA    R4,4000(R4)         BUMP TO NEXT RECORD ADDRESS                  
         LA    R2,1(R2)            BUMP PAGE NUMBER                             
         BCT   R0,GETSAVE2         DO FOR NUMBER OF TWA'S                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                               VALIDATE STATION EXPRESSION ON SCREEN           
VALSTA   NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         XC    ACTSTAT,ACTSTAT                                                  
*                                                                               
         LA    R4,BLOCK                                                         
         XC    0(64,R4),0(R4)      CLEAR SCANNER TABLE                          
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),C',=/-'                               
*                                                                               
         TM    2(R4),X'80'         TEST VALID NUMERIC                           
         BZ    STA05                                                            
         MVI   ERROR,INVMKT                                                     
         CLI   0(R4),4             YES - INPUT IS MARKET NUMBER                 
         BH    EDTERR                                                           
         MVC   ACTMKT,6(R4)                                                     
*                                                                               
         MVI   DBFUNCT,DBGETMK     VALIDATE  MARKET                             
         MVC   DBAREC,AIO2                                                      
         MVC   DBSELRMK,ACTMKT                                                  
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,0                                                        
         BNE   EDTERR                                                           
         GOTO1 DEFINE,DMCB,=C'MNAME',DBLOCK,WORK                                
         MVC   STAMKT(L'STAMKT),WORK+2            MARKET NAME                   
         OI    STAMKTH+6,X'80'                                                  
*                                                                               
         SR    RE,RE               SET ACTSTAT TO MARKET NUMBER                 
         ICM   RE,3,ACTMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ACTSTAT(4),DUB                                                   
         MVI   ACTSTAT+4,C'A'                                                   
         B     XIT                                                              
*                                                                               
STA05    CLI   0(R4),3             INPUT IS NOT MARKET NUMBER                   
         BL    EDTERR                                                           
         CLI   0(R4),4                                                          
         BH    EDTERR                                                           
         TM    2(R4),X'40'         TEST ALPHA (MSPACK REQUIREMENT)              
         BZ    EDTERR                                                           
         MVC   ACTSTAT(4),12(R4)                                                
*                                                                               
         CLI   1(R4),2                                                          
         BH    EDTERR                                                           
         CLI   22(R4),C' '                                                      
         BNE   *+14                                                             
         MVC   22(2,R4),=C'T '     SET FOR THE TELE                             
         MVI   1(R4),X'01'         FUDGE THE SCANNER BLOCK                      
         MVC   ACTSTAT+4(1),22(R4)                                              
         ZIC   R5,1(R4)                                                         
         BCTR  R5,0                                                             
         EX    R5,STAAM                                                         
         BE    STA10                                                            
         EX    R5,STAFM                                                         
         BE    STA10                                                            
         EX    R5,STACO                                                         
         BE    STA10                                                            
         EX    R5,STAT                                                          
         BE    STA10                                                            
         B     EDTERR                                                           
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
STACO    CLC   22(0,R4),=C'CO'                                                  
STAT     CLC   22(0,R4),=C'T '                                                  
         SPACE 1                                                                
* VALIDATE STATION CALL LETTERS EXIST ON DEMO FILE *                            
         SPACE 1                                                                
STA10    DS    0H                                                               
*                                                                               
MKTSTA   DS    0H                  VAL STATIONS ON DEMO FILE                    
         MVI   DBFUNCT,DBVLST                                                   
         MVC   DBAREC,AIO2                                                      
         MVC   DBSELSTA,ACTSTAT    VALIDATE CALL LETTERS                        
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         CLI   DBERROR,0                                                        
         BNE   MKT10                                                            
         B     XIT                                                              
MKT10    MVI   ERROR,INVSTAT                                                    
         B     EDTERR                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
TRAPERR  GOTO1 VGETERR                                                          
EDTERR   EQU   TRAPERR                                                          
*                                                                               
*                                                                               
BUMP     SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST E-O-S                                   
         BR    RE                                                               
         SPACE 1                                                                
LISTHD   DC    C'AVAILABLE BOOKS                      AVAILABLE BOOKS'          
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESC3D                                                       
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* WORK AREA                                                                     
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
BINSRCH  DS    V                                                                
ENDNEAR  DS    C                                                                
OPTNO    DS    XL2                 NUMBER GIVEN ON WHICH TO FILTER              
BOOK     DS    XL2                                                              
RADMED   DS    CL1                                                              
LISTNQ   EQU   X'0E'                                                            
*                                                                               
BINPARMS DS    0F                  BINSRCH PARAMETER LIST                       
BINCMND  DS    0X                  COMMAND                                      
BINAREC  DS    A                   A(RECORD)                                    
BINATAB  DS    A                   A(TABLE)                                     
BINSOFAR DS    A                   N'ENTRIES SO FAR                             
BINLREC  DS    F                   L'RECORD                                     
BINDKEY  DS    0X                  DISPLACEMENT TO KEY                          
BINLKEY  DS    F                   L'KEY                                        
BINMAXN  DS    F                   MAXIMUM N'TABLE ENTRIES                      
BINLAST  DS    F                   SAVED BINNEXT VALUE                          
BINPARML EQU   *-BINPARMS                                                       
*                                                                               
POSTLINE DS    XL80                INPUT/OUTPUT POSTING LINE                    
         DS    CL(L'OVWORK-(*-OVWORK))           SPARE                          
*                                                                               
T20FFFD  DSECT                                                                  
         ORG   CONHEAD+3520-256                                                 
*                                                                               
SVTBLNDX DS    F                                                                
SVTBLSIZ DS    F                                                                
LISTN    DS    CL1                                                              
         DS    CL247               SPARE                                        
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*DSECT TO COVER LIST LINE                                                       
*                                                                               
LSTLINE  DSECT                                                                  
         DS    CL5                                                              
LSTBOOK  DS    CL4                 FIRST BOOK                                   
         DS    CL30                                                             
LSTBOOK2 DS    CL4                 SECOND BOOK                                  
*                                                                               
*                                                                               
*DSECT TO COVER MARKET RECORDS                                                  
*                                                                               
MARRECD  DSECT                                                                  
MARKET   DS    XL2                 MARKET NUMBER                                
MARNAME  DS    CL30                MARKET NAME                                  
MAREND   EQU   *                                                                
*                                                                               
*                                                                               
* DSECT TO COVER BINSRCH RECORD                                                 
*                                                                               
BINRECD  DSECT                                                                  
BINBOOK  DS    XL2                 BOOK DATE                                    
BINEND   EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027SPRES23S  05/01/02'                                      
         END                                                                    

*          DATA SET SPRES22    AT LEVEL 034 AS OF 05/01/02                      
*PHASE T20F22B,*                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE KHDUMMY                                                                
         TITLE 'T20F22- RESEARCH MARKET/BOOK LIST'                              
T20F22   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20F22,RR=R2                                                   
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
         MVC   CTRY,SVCTRY                                                      
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
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
         TM    MKTMEDH+4,X'20'     MEDIA                                        
         BZ    VK00                                                             
         TM    MKTSRCH+4,X'20'     SOURCE FIELD                                 
         BZ    VK00                                                             
         TM    MKTBOKH+4,X'20'                                                  
         BZ    VK00                                                             
         TM    MKTOPTH+4,X'20'     OPTION FIELD                                 
*^^NOP   BO    XIT                                                              
         BZ    VK00                                                             
         B     VK00_A              ALL WAS VLDTED PRVSLY--DON'T XIT YET         
*                                                                               
VK00     DS    0H                                                               
         MVI   BEENHERE,0                                                       
                                                                                
VK00_A   DS    0H                                                               
         XC    BOOKTYPE,BOOKTYPE   GET BOOKTYPE                                 
         LA    R2,MKTBOK                                                        
         CLI   MKTBOKH+5,4         MINIMUM FOR A BOOK                           
         BNH   VK01                                                             
         ZIC   R5,MKTBOKH+5                                                     
         BCTR  R5,0                                                             
         AR    R2,R5                                                            
         CLI   0(R2),C')'          DOES IT HAVE A BOOKTYPE?                     
         BNE   VK01                                                             
         BCTR  R2,0                                                             
         MVC   BOOKTYPE,0(R2)                                                   
VK01     LA    R2,MKTMEDH                                                       
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
VK10     LA    R2,MKTSRCH          VALIDATE SOURCE FIELD                        
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
*                                                                               
         GOTO1 VVALSRC                                                          
         MVC   SRCTYPE,DBSELSRC                                                 
*                                                                               
         CLI   RADMED,C'Y'                                                      
         BNE   VK20                                                             
         MVI   DBSELMED,C'R'                                                    
*              NOTICE:LOSS OF RAD BAGYMED VALUE DUE TO MEDIA FUDGE              
*                                                                               
VK20     LA    R2,MKTBOKH          VALIDATE BOOK FIELD                          
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     EDTERR                                                           
*                                                                               
         CLI   RADMED,C'Y'         TEST RADIO MEDIA                             
         BNE   VK25                                                             
         GOTO1 VRADBOOK                                                         
         B     VK27                                                             
*                                                                               
VK25     GOTO1 VVALBOOK                                                         
*                                                                               
VK27     LA    R1,BOOKS                                                         
         MVC   BOOK(2),1(R1)       MOVE BINARY BOOK TO STORAGE                  
*                                                                               
VK30     LA    R2,MKTOPTH          VALIDATE OPTION FIELD                        
         CLI   5(R2),0                                                          
         BE    VK60                                                             
*                                                                               
         TM    4(R2),X'04'         VALID ALPHA                                  
         BZ    VK50                                                             
         CLI   5(R2),8                                                          
         BNH   VK60                                                             
*                                                                               
VK40     MVI   ERROR,TOOLONGA      OPTION INPUT TOO LONG                        
         B     EDTERR                                                           
*                                                                               
VK50     TM    4(R2),X'08'         VALID NUMERIC                                
         BO    *+12                                                             
         MVI   ERROR,INVINPT       INVALID INPUT                                
         B     EDTERR                                                           
         MVI   NUMERC,C'Y'                                                      
         CLI   5(R2),4                                                          
         BNH   *+12                                                             
         MVI   ERROR,TOOLONGN                                                   
         B     EDTERR                                                           
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STH   R1,OPTNO            CONVERT MKT# OPTION TO BINARY                
*                                                                               
VK60     DS    0H                                                               
         OI    MKTMEDH+4,X'20'     MEDIA                                        
         OI    MKTSRCH+4,X'20'     SOURCE FIELD                                 
         OI    MKTBOKH+4,X'20'                                                  
         OI    MKTOPTH+4,X'20'     OPTION FIELD                                 
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*              FORMAT ONLINE LIST SCREEN                                        
*                                                                               
LR       MVC   MEDTYPE,MKTMED                                                   
*        MVC   SRCTYPE,MKTSRC                                                   
         CLI   BEENHERE,0                                                       
         BNE   *+14                                                             
         OC    KEY(18),KEY         TEST FIRST TIME THROUGH                      
         BZ    LR10                OR CHAR INCL DISK ADD                        
         BAS   RE,GETSAVE                                                       
         LA    R1,BUFF                                                          
         ST    R1,BINATAB                                                       
         L     R1,SVTBLNDX                                                      
         ZIC   R0,LISTN                                                         
         AR    R1,R0               BUMP TABLE INDEX ON NEW SCREEN               
         LA    R1,1(R1)            TOP OF NEW SCREEN                            
         ST    R1,SVTBLNDX                                                      
         C     R1,SVTBLSIZ         TEST END OF TABLE                            
         BH    LR100                                                            
         B     LR20                                                             
*                                                                               
LR10     XC    SVTBLNDX,SVTBLNDX                                                
         XC    SVTBLSIZ,SVTBLSIZ                                                
         MVI   BEENHERE,1                                                       
         BAS   RE,DEMPROC          BUILD THE LIST TABLE                         
         MVC   SVTBLSIZ,BINSOFAR    TABLE SIZE=#MARKETS                         
         MVI   LISTN,LISTNQ        SET #LIST LINES                              
         MVC   MKTHDLN(L'LISTHD),LISTHD        MOVE IN HEADLINE                 
         OI    MKTHDLNH+6,X'80'    TRANSMIT                                     
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
         OC    SVTBLNDX,SVTBLNDX   TEST FISRT TIME THROUGH                      
         BNZ   LR40                                                             
         SR    RE,RE                                                            
         ICM   RE,3,BINMNAM+L'BINMNAM-2                                         
         EDIT  (RE),(4,LSTMKTN),ALIGN=LEFT                                      
         MVC   LSTMKTN+4(20),=C' ACTIVE MKTS IN BOOK'                           
         B     LR45                                                             
*                                                                               
LR40     CLC   BINMRKT(6),=6X'FF'       TEST FOR END OF LIST                    
         BE    LR100                                                            
*                                                                               
         MVC   LSTMKTN,BINMNAM                                                  
         SR    RE,RE                                                            
         ICM   RE,3,BINMRKT                                                     
         EDIT  (RE),(4,LSTMKT),ALIGN=LEFT                                       
         STH   RE,MKTNUM                                                        
         BAS   RE,ALPHMRKT                                                      
         MVC   LSTALPH,CITYCODE                                                 
*                                                                               
         CLI   ENDNEAR,C'Y'                                                     
         BE    LR50                NO NEED TO DO SECOND COLUMN                  
*                                                                               
LR45     ZIC   RE,LISTN                                                         
         LA    RE,1(RE)                                                         
         MH    RE,=Y(BINEND-BINRECD)     BUMP AHEAD 16 ENTRIES                  
         AR    R7,RE                                                            
*                                                                               
         CLC   BINMRKT(6),=6X'FF'        TEST FOR END OF LIST                   
         BNE   *+12                                                             
         MVI   ENDNEAR,C'Y'                                                     
         B     LR50                                                             
*                                                                               
         OC    BINMNAM,BINMNAM     SEE IF ANY DATA                              
         BZ    LR50                                                             
         MVC   LSTMKTN2,BINMNAM                                                 
         SR    RE,RE                                                            
         ICM   RE,3,BINMRKT                                                     
         EDIT  (RE),(4,LSTMKT2),ALIGN=LEFT                                      
         STH   RE,MKTNUM                                                        
         BAS   RE,ALPHMRKT                                                      
         MVC   LSTALPH2,CITYCODE                                                
*                                                                               
LR50     L     R1,SVTBLNDX                                                      
         LA    R1,1(R1)                                                         
         ST    R1,SVTBLNDX         BUMP NEXT ENTRY                              
         MVC   KEY(13),=CL13'TABLE INDEX  '                                     
         XC    KEY+14(2),KEY+14    CLEAR DISC ADDRESS                           
         MVC   KEY+16(2),=2X'01'                                                
         MVC   DMDSKADD,KEY+14     SET DISK ADD FOR GENCON                      
         XC    DMDSKADD,DMDSKADD   SET DISK TO BYPASS GETREC                    
*                                                                               
         DROP  R7                                                               
*                                                                               
LR80     GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR20                                                             
*                                                                               
LR100    XC    KEY,KEY                                                          
         NI    MKTMEDH+4,X'FF'-X'20' SET MEDIA VALIDATED OFF                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*              FORMAT OFFLINE REPORT                                            
*                                                                               
PR       DS    0H                                                               
         MVC   MEDTYPE,MKTMED                                                   
*        MVC   SRCTYPE,MKTSRC                                                   
         CLI   MODE,PRINTREP       TEST REPORT MODE                             
         BE    PR10                                                             
         MVI   ERROR,INVACT        MUST BE OFFLINE TO REPORT                    
         B     EDTERR                                                           
*                                                                               
PR10     LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   HDHOOKOK,C'Y'                                                    
*                                                                               
         DS    0H                                                               
         MVI   PRINTN,49           #LINES ON PAGE                               
         XC    SVTBLSIZ,SVTBLSIZ                                                
         XC    SVTBLNDX,SVTBLNDX                                                
         BAS   RE,DEMPROC          BUILD THE MARKET REC TABLE                   
         MVC   SVTBLSIZ,BINSOFAR    TABLE SIZE=#MARKETS                         
         AF    SVTBLNDX,=F'1'                                                   
*                                                                               
PR20     DS    0H                                                               
         CLI   LASTLN,C'Y'                                                      
         BNE   PR30                                                             
         MVI   LASTLN,0                                                         
         AF    SVTBLNDX,=F'98'     BUMP TABLE INDEX                             
*        ZAP   LINE,=P'99'         FORCE NEW PAGE                               
         MVI   LINE,99             FORCE NEW PAGE                               
         B     PR35                                                             
*                                                                               
PR30     XR    R1,R1                                                            
         L     R0,SVTBLNDX                                                      
         SRDL  R0,32                                                            
         D     R0,=F'49'           TEST AT END OF PAGE                          
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         MVI   LASTLN,C'Y'                                                      
*                                                                               
PR35     CLC   SVTBLNDX,SVTBLSIZ   TEST END OF TABLE                            
         BH    PR100                                                            
         L     R7,BINATAB          POINT TO TABLE                               
         USING BINRECD,R7                                                       
         L     R0,SVTBLNDX                                                      
         MH    R0,=Y(BINEND-BINRECD)                                            
         AR    R7,R0               POINT TO ENTRY IN TABLE                      
*                                                                               
         LA    R2,P1                                                            
         MVC   P1,SPACES                                                        
         USING PRTLINE,R2                                                       
         OC    SVTBLNDX,SVTBLNDX                                                
         BNZ   PR40                                                             
         SR    RE,RE                                                            
         ICM   RE,3,BINMNAM+L'BINMNAM-2                                         
         EDIT  (RE),(4,PRTMKTN),ALIGN=LEFT                                      
         MVC   PRTMKTN+4(23),=C' ACTIVE MARKETS IN BOOK'                        
         B     PR45                                                             
*                                                                               
PR40     CLC   BINMRKT(6),=6X'FF'       TEST FOR END OF LIST                    
         BE    PR100                                                            
*                                                                               
         MVC   PRTMKTN,BINMNAM                                                  
         SR    RE,RE                                                            
         ICM   RE,3,BINMRKT                                                     
         EDIT  (RE),(4,PRTMKT),ALIGN=LEFT                                       
         STH   RE,MKTNUM                                                        
         BAS   RE,ALPHMRKT                                                      
         MVC   PRTALPH,CITYCODE                                                 
*                                                                               
         CLI   ENDNEAR,C'Y'                                                     
         BE    PR60                NO NEED TO DO SECOND COLUMN                  
*                                                                               
PR45     ZIC   RE,PRINTN           #LINES ON PAGE                               
         MH    RE,=Y(BINEND-BINRECD)     BUMP AHEAD 50 ENTRIES                  
         AR    R7,RE                                                            
*                                                                               
         CLC   BINMRKT(6),=6X'FF'        TEST FOR END OF LIST                   
         BNE   *+12                                                             
         MVI   ENDNEAR,C'Y'                                                     
         B     PR60                                                             
*                                                                               
         OC    BINMNAM,BINMNAM     SEE IF ANY DATA                              
         BZ    PR60                                                             
         MVC   PRTMKTN2,BINMNAM                                                 
         SR    RE,RE                                                            
         ICM   RE,3,BINMRKT                                                     
         EDIT  (RE),(4,PRTMKT2),ALIGN=LEFT                                      
         STH   RE,MKTNUM                                                        
         BAS   RE,ALPHMRKT                                                      
         MVC   PRTALPH2,CITYCODE                                                
*                                                                               
         CLI   ENDNEAR2,C'Y'       TEST END IN 3RD COLUMN                       
         BE    PR60                                                             
*                                                                               
PR50     ZIC   RE,PRINTN           #LINES ON PAGE                               
         MH    RE,=Y(BINEND-BINRECD)     BUMP AHEAD 50 ENTRIES                  
         AR    R7,RE                                                            
*                                                                               
         CLC   BINMRKT(6),=6X'FF'        TEST FOR END OF LIST                   
         BNE   *+12                                                             
         MVI   ENDNEAR2,C'Y'                                                    
         B     PR60                                                             
*                                                                               
         OC    BINMNAM,BINMNAM     SEE IF ANY DATA                              
         BZ    PR60                                                             
         MVC   PRTMKTN3,BINMNAM                                                 
         SR    RE,RE                                                            
         ICM   RE,3,BINMRKT                                                     
         EDIT  (RE),(4,PRTMKT3),ALIGN=LEFT                                      
         STH   RE,MKTNUM                                                        
         BAS   RE,ALPHMRKT                                                      
         MVC   PRTALPH3,CITYCODE                                                
*                                                                               
PR60     L     R1,SVTBLNDX                                                      
         LA    R1,1(R1)                                                         
         ST    R1,SVTBLNDX         BUMP NEXT ENTRY                              
         DROP  R7                                                               
*                                                                               
PR80     GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR20                                                             
*                                                                               
PR100    DS    0H                                                               
         XC    KEY,KEY                                                          
         NI    MKTMEDH+4,X'FF'-X'20' SET MEDIA VALIDATED OFF                    
         B     XIT                                                              
         EJECT                                                                  
MEDTYPE  DC    C' '                                                             
SRCTYPE  DC    C' '                                                             
BOOKTYPE DC    X'00'                                                            
MKTNUM   DC    H'0'                                                             
CODETABL DC    CL5' '                                                           
CITYCODE DC    CL3' '                                                           
*-------------------------------------------------------------                  
* THIS PROCEDURE IS TO FIND THE ALPHA MARKET FOR A GIVEN                        
* MARKET NUMBER.                                                                
* THE INPUTS NEEDED ARE: MARKET NUMBER    (MKTNUM)                              
*                        MEDIA TYPE       (MEDTYPE)                             
*                        RATING SOURCE    (SRCTYPE)                             
*                        BOOK TYPE        (BOOKTYPE)                            
* THE OUTPUT WILL BE AN ALPHA MARKET THAT GOES IN CITYCODE                      
*-------------------------------------------------------------                  
ALPHMRKT NTR1                                                                   
         MVI   DBFUNCT,DBCNVN2A                                                 
         MVC   DBSELRMK,MKTNUM     SET NUMERIC MKT IN DBLOCK                    
         MVC   DBAREC,AIO1         SET SOME OTHER DBLOCK FLDS                   
         MVC   DBSELBK,BOOK        CNTRLR SETS FILE/MEDIA/SRC                   
         MVC   DBBTYPE,BOOKS+3                                                  
         GOTO1 DEMAND,DMCB,DBLOCK,0,0                                           
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CITYCODE,DBSELALF    SET ALPHA MARKET IN TABLE                   
         B     XIT                                                              
         SPACE 1                                                                
*                                                                               
*        LA    R5,CODETABL                                                      
*        MVC   0(3,R5),MEDTYPE                                                  
*        MVC   3(2,R5),MKTNUM                                                   
*        GOTO1 VNUM2CDE                                                         
*        B     XIT                                                              
                                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                         PROCEDURE TO CALL DEMAND TO GET MKT RECORDS           
*                                                                               
DEMPROC  NTR1                                                                   
         MVC   DBAREC,AIO1                                                      
         MVI   DBFUNCT,DBGETMKB    GET MARKET REC FOR BOOK                      
         MVC   DBSELBK,BOOK                                                     
         MVC   DBBTYPE,BOOKS+3                                                  
         XC    MARKETN,MARKETN     CLEAR N'MARKETS                              
*                                                                               
         LA    R0,POSTLINE         SET PARAMETERS FOR BINSRCH                   
         ST    R0,BINAREC                                                       
         CLI   MODE,PRINTREP                                                    
         BE    DPROC                                                            
*                                  BINPARMS FOR LIST                            
         LA    R0,BUFF             TABLE SET IN 8K BUFFER                       
         ST    R0,BINATAB                                                       
         L     R1,=AL4(BUFFEND-BUFF)      SET MAX# REC IN BUFFER                
         SR    R0,R0                                                            
         LA    RE,BINEND-BINRECD                                                
         DR    R0,RE                                                            
         ST    R1,BINMAXN                                                       
         B     DPROC10                                                          
*                                  BINPARMS FOR REPORT                          
DPROC    LA    R0,REPBUFF          SET TABLE AREA FOR OFFLINE                   
         ST    R0,BINATAB                                                       
         L     R1,=AL4(REPEND-REPBUFF)   SET MAX# REC IN STORAGE                
         SR    R0,R0                                                            
         LA    RE,BINEND-BINRECD                                                
         DR    R0,RE                                                            
         ST    R1,BINMAXN                                                       
*                                                                               
DPROC10  XC    BINSOFAR,BINSOFAR                                                
         LA    R0,BINEND-BINRECD    LENGTH OF RECORD                            
         ST    R0,BINLREC                                                       
*                                                                               
         CLI   NUMERC,C'Y'          TEST NUMERIC OPTION                         
         BE    DEM1                                                             
         LA    R0,BINMNAM-BINRECD                                               
         LA    RF,L'BINMNAM                                                     
         ST    RF,BINLKEY                                                       
         STC   R0,BINDKEY          DEFAULT SORT ALPHA                           
         B     DEM2                                                             
DEM1     LA    R0,BINMRKT-BINRECD                                               
         LA    RF,BINMNAM-BINRECD                                               
         ST    RF,BINLKEY                                                       
         STC   R0,BINDKEY          NUMERIC SORT                                 
*                                                                               
*                                  CALL DEMAND TO READ RECORDS                  
DEM2     GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK,0                                     
         LA    R2,POSTLINE                                                      
         USING BINRECD,R2                                                       
         XC    BINMRKT,BINMRKT                                                  
         XC    BINMNAM,BINMNAM                                                  
         MVC   BINMNAM+L'BINMNAM-2(2),MARKETN                                   
         BAS   RE,POST                                                          
         XC    BINMRKT,BINMRKT                                                  
         XC    BINMNAM,BINMNAM                                                  
         MVC   BINMRKT(6),=6X'FF'         CREATE END OF LIST MARKER             
DEM3     BAS   RE,POST                                                          
         L     R1,BINSOFAR         ADD FIRST AND LAST LINE TO TAB COUNT         
         AH    R1,=H'2'                                                         
         ST    R1,BINSOFAR                                                      
         CLC   BINSOFAR,BINMAXN                                                 
         BL    DEM3                                                             
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* ROUTINE TO PROCESS A DEMO RECORD RETURNED FROM DEMAND                         
*                                                                               
DEMHOOK  ST    RE,SAVERE           SAVE RETURN ADDRESS                          
*                                  SEARCH RECORD FOR NAME ELEMENT               
         L     R1,DBAREC                                                        
         LA    R1,DMFRSTEL-DMKEY(R1)                                            
         USING DMELEM,R1           R1=A(FIRST ELEMENT)                          
         SR    RE,RE                                                            
DEMHOOK2 CLI   DMELEM,0            TEST E-O-R                                   
         BE    DEMHOOKX                                                         
         CLI   DMELEM,DMECODEQ     TEST MARKET NAME ELEMENT                     
         BE    *+14                                                             
         IC    RE,DMLEN                                                         
         AR    R1,RE                                                            
         B     DEMHOOK2                                                         
*                                  EXTRACT MARKET NAME FROM ELEMENT             
         LA    R7,WORK                                                          
         USING MARRECD,R7                                                       
         MVC   MARNAME,SPACES                                                   
         MVC   MARKET,DMMNO                                                     
         IC    RE,DMLEN                                                         
         SH    RE,=H'5'            4 BYTES GIVEN+1 BYTE MVC LEN                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MARNAME(0),DMMNAME                                               
         DROP  R1                                                               
*                                                                               
*                                                                               
         LA    R2,MKTOPTH          POINT TO OPTION FIELD                        
         CLI   5(R2),0             TEST FOR INPUT                               
         BE    DEMHOOK4                                                         
         CLI   NUMERC,C'Y'                                                      
         BNE   DEMHOOK3                                                         
         CLC   MARKET,OPTNO        BINARY FORM OF OPTION#                       
         BNL   DEMHOOK4                                                         
         B     DEMHOOK6                                                         
*                                                                               
DEMHOOK3 ZIC   RE,5(R2)            INPUT LENGTH IN RE                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   MARNAME(0),8(R2)    MATCH ON INPUT OPTION                        
         BL    DEMHOOKX            NONE LESS THAN                               
*                                  BUILD BINSRCH RECORD & POST                  
DEMHOOK4 LA    R4,POSTLINE                                                      
         USING BINRECD,R4                                                       
         MVC   BINMNAM,MARNAME                                                  
         MVC   BINMRKT,MARKET                                                   
         BAS   RE,POST                                                          
         DROP  R4                                                               
*                                  BUMP N'ACTIVE MARKETS                        
DEMHOOK6 SR    R1,R1                                                            
         ICM   R1,3,MARKETN                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,MARKETN                                                     
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
         CLC   BINSOFAR,BINMAXN    TEST TABLE FULL                              
         BL    POST2               NO - GO AHEAD                                
*                                                                               
         L     R1,BINATAB          YES - R1 = A(TABLE)                          
         L     RF,BINSOFAR         RF = ENTRIES SO FAR                          
         BCTR  RF,0                                                             
         SR    RE,RE                                                            
         M     RE,BINLREC          RF = DSPL OF LAST RECORD IN TABLE            
         AR    R1,RE               R1 = A(LAST RECORD)                          
         ZIC   RE,BINDKEY          RE = DSPL TO KEY                             
         AR    R1,RE               R1 = A(KEY OF LAST RECORD)                   
         L     RF,BINAREC                                                       
         AR    RF,RE               RF = A(KEY OF INCOMING RECORD)               
         ICM   RE,7,BINLKEY+1                                                   
         BCTR  RE,0                RE = LENGTH OF KEY - 1                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),0(R1)       COMPARE INCOMING KEY TO LAST KEY             
         BNL   POSTX               NOT LOW - FORGET IT                          
*                                                                               
         L     R1,BINSOFAR         LOW - TRASH THE LAST ENTRY                   
         BCTR  R1,0                                                             
         ST    R1,BINSOFAR                                                      
*                                                                               
POST2    GOTO1 BINSRCH,BINPARMS    CALL BINSRCH                                 
         OC    BINAREC+1(3),BINAREC+1                                           
         BNZ   POSTX                                                            
         DC    H'0'                (SHOULD NEVER OCCUR)                         
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
         BCT   R0,GETSAVE2         DO FOR LEN OF BUFF (8K)                      
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
         CLI   HDHOOKOK,C'N'       TEST OK TO DO HEADHOOK                       
         BE    HOOKX                                                            
*                                                                               
         MVC   H3+1(5),=C'MEDIA'                                                
         LA    R2,MKTMEDH                                                       
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   H3+8(0),8(R2)       MOVE IN MEDIA                                
         MVC   H3+11(6),=C'SOURCE'                                              
         LA    R2,MKTSRCH                                                       
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   H3+19(0),8(R2)      MOVE IN SOURCE                               
         MVC   H4+1(4),=C'BOOK'                                                 
         LA    R2,MKTBOKH                                                       
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   H4+8(0),8(R2)      MOVE IN BOOK                                  
         MVI   H5,0                                                             
*                                                                               
         LA    R5,H6                                                            
         USING PRTLINE,R5                                                       
         MVC   PRTMKT(6),=C'MKT NO'           INSERT TABLE HEADLINES            
         MVC   PRTMKTN+4(11),=C'MARKET NAME'                                    
         MVC   PRTMKT2(6),=C'MKT NO'                                            
         MVC   PRTMKTN2+4(11),=C'MARKET NAME'                                   
         MVC   PRTMKT3(6),=C'MKT NO'                                            
         MVC   PRTMKTN3+4(11),=C'MARKET NAME'                                   
*                                                                               
*                                                                               
         OC    ABOX,ABOX                                                        
         BZ    HOOKX                                                            
*                                                                               
         L     R4,ABOX             ADDRESS OF BOX DSECT                         
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXROWS+59,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+2,C'L'                                                   
         MVI   BOXCOLS+9,C'C'                                                   
         MVI   BOXCOLS+42,C'C'                                                  
         MVI   BOXCOLS+50,C'C'                                                  
         MVI   BOXCOLS+84,C'C'                                                  
         MVI   BOXCOLS+91,C'C'                                                  
         MVI   BOXCOLS+120,C'R'                                                 
*                                                                               
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R4,R5                                                            
*                                                                               
HOOKX    B     XIT                                                              
*                                                                               
HEDSPECS SSPEC H1,2,C'MEDIA     SPOT RESEARCH'                                  
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,53,C'MARKET LIST REPORT'                                      
         SSPEC H2,53,C'------------------'                                      
         SSPEC H1,102,RUN                                                       
         SSPEC H2,102,REPORT                                                    
         SSPEC H2,120,PAGE                                                      
         DC    H'0'                                                             
         EJECT                                                                  
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
LISTHD   DC    C' MARKET     NAME                      MARKET     NAME'         
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         LTORG                                                                  
         SPACE 10                                                               
REPBUFF  DS    10000C              OFFLINE REPORT BUFFER                        
REPEND   EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DEALPHAMKD                                                     
       ++INCLUDE SPRESWORKD                                                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPRESC2D                                                       
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* WORK AREA                                                                     
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
BINSRCH  DS    V                                                                
NUMERC   DS    C                                                                
ENDNEAR  DS    C                                                                
ENDNEAR2 DS    C                                                                
HDHOOKOK DS    C                                                                
OPTNO    DS    XL2                 NUMBER GIVEN ON WHICH TO FILTER              
MARKETN  DS    XL2                 #ACTIVE MARKETS FOR A BOOK                   
BOOK     DS    XL2                                                              
RADMED   DS    CL1                                                              
PRINTN   DS    CL1                                                              
LASTLN   DS    CL1                                                              
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
SVTBLNDX DS    F                   SAVED TABLE INDEX                            
SVTBLSIZ DS    F                   SAVED TABLE SIZE                             
LISTN    DS    CL1                                                              
BEENHERE DS    CL1                                                              
         DS    CL246               SPARE                                        
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
         DS    CL3                                                              
LSTMKT   DS    CL4                 MARKET NUMBER                                
         DS    CL1                 MARKET NUMBER                                
LSTALPH  DS    CL3                                                              
         DS    CL1                 MARKET NUMBER                                
LSTMKTN  DS    CL22                MARKET NAME                                  
         DS    CL6                                                              
LSTMKT2  DS    CL4                                                              
         DS    CL1                 MARKET NUMBER                                
LSTALPH2 DS    CL3                                                              
         DS    CL1                 MARKET NUMBER                                
LSTMKTN2 DS    CL22                                                             
*                                                                               
*                                                                               
*                                                                               
*DSECT TO COVER PRINT LINE                                                      
*                                                                               
PRTLINE  DSECT                                                                  
         DS    CL3                                                              
PRTMKT   DS    CL4                 MARKET NUMBER                                
         DS    CL2                                                              
PRTALPH  DS    CL3                                                              
         DS    CL2                                                              
PRTMKTN  DS    CL24                MARKET NAME                                  
         DS    CL9                                                              
PRTMKT2  DS    CL4                                                              
         DS    CL2                                                              
PRTALPH2 DS    CL3                                                              
         DS    CL2                                                              
PRTMKTN2 DS    CL24                                                             
         DS    CL9                                                              
PRTMKT3  DS    CL4                                                              
         DS    CL2                                                              
PRTALPH3 DS    CL3                                                              
         DS    CL2                                                              
PRTMKTN3 DS    CL24                                                             
         DS    CL6                                                              
*                                                                               
*                                                                               
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
BINMRKT  DS    XL2                 MARKET NUMBER                                
BINMNAM  DS    CL24                MARKET NAME                                  
BINEND   EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034SPRES22   05/01/02'                                      
         END                                                                    

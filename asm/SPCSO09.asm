*          DATA SET SPCSO09    AT LEVEL 062 AS OF 05/01/02                      
*PHASE T21809A                                                                  
         TITLE 'T21809 - CHILD SPOT DEMO OVERRIDE MAINTENANCE'                  
T21809   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21809                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MYOVNUM,X'09'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    OVRMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'09'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE AND DISPLAY RECORDS                 
         BE    VK                                                               
         DC    H'0'                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       MVI   KEYCHANG,C'N'                                                    
*                                                                               
VKMED    LA    R2,OVRMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    OVRCLTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,OVRCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKSTA                                                            
         NI    OVRSTAH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKSTA    LA    R2,OVRSTAH          VALIDATE MARKET FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    OVRESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKEST    LA    R2,OVRESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKREF                                                            
         NI    OVRREFH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
VKREF    LA    R2,OVRREFH          VALIDATE REFERENCE FIELD                     
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         MVI   KEYCHANG,C'Y'                                                    
         GOTO1 ANY                                                              
         GOTO1 VALIREF                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKX      LA    R6,KEY              BUILD KEY                                    
         USING DEMKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   DEMKTYPE,DEMKTYPQ   DEMO OVERRIDE RECORD TYPE                    
         MVI   DEMKSTYP,DEMKSTPQ   DEMO OVERRIDE RECORD SUB-TYPE                
         MVC   DEMKAM,BAGYMD                                                    
         MVC   DEMKCLT,BCLT                                                     
         MVC   DEMKMKT(5),BMKTSTA                                               
         MVC   DEMKEST,BMEST                                                    
         MVC   DEMKREF,BREF                                                     
         DROP  R6                                                               
*                                                                               
         BAS   RE,CHKCOPY          TEST FOR COPY OPTION                         
*                                                                               
         MVI   RECFOUND,C'N'                                                    
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                TEST RECORD EXISTS                           
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VKX10                                                            
         OI    DMINBTS,X'08'                                                    
         GOTO1 GETREC              IF IT DOES READ IT IN                        
         NI    DMINBTS,X'F7'                                                    
         MVI   RECFOUND,C'Y'                                                    
*                                                                               
VKX10    CLI   KEYCHANG,C'Y'       IF KEY CHANGED THEN DISPLAY ONLY             
         BE    DR                                                               
         B     VR                  ELSE VALIDATE AND DISPLAY                    
         EJECT                                                                  
* THIS ROUTINE LOOKS IN THE OPTION FIELD FOR THE OPTION 'COPY=EST/REF'.         
* IF THE OPTION IS THERE THEN THE SPECIFIED EST/REF PAIR WILL BE COPIED         
* TO THE EST/REF PAIR SPECIFED IN THE KEY FIELDS.                               
*                                                                               
CHKCOPY  NTR1                                                                   
         LA    R2,OVROPTH          DONE IF BLANK FIELD                          
         CLI   5(R2),0                                                          
         BE    CPX                                                              
*                                                                               
         LA    R5,SCANOUT          SCAN FIELD INTO SCANOUT                      
         GOTO1 SCANNER,DMCB,(R2),(R5),C',==/'                                   
         CLI   4(R1),2             MUST HAVE 2 BLOCKS                           
         BNE   INVERR                                                           
         CLI   0(R5),4             FIRST BLOCK MUST SAY 'COPY'                  
         BNE   INVERR                                                           
         CLC   12(4,R5),=C'COPY'                                                
         BNE   INVERR                                                           
         CLI   1(R5),0             AND HAVE NO SECOND HALF                      
         BNE   INVERR                                                           
*                                                                               
         LA    R5,32(R5)           BUMP TO SECOND BLOCK                         
         TM    2(R5),X'80'         BOTH HALVES MUST BE NUMERIC                  
         BZ    INVERR                                                           
         CLC   4(4,R5),=F'256'     AND < 256                                    
         BNL   INVERR                                                           
         TM    3(R5),X'80'                                                      
         BZ    INVERR                                                           
         CLC   8(4,R5),=F'256'                                                  
         BNL   INVERR                                                           
*                                                                               
         LA    R4,KEY              R4 = A(KEY)                                  
         USING DEMKEY,R4                                                        
         L     R6,AIO              R6 = A(IOAREA)                               
*                                                                               
         MVC   DEMKEST,7(R5)       READ FOR SOURCE RECORD                       
         MVC   DEMKREF,11(R5)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRRNF              ERROR IF NOT FOUND                           
*                                                                               
         GOTO1 GETREC              READ SOURCE RECORD                           
*                                                                               
         MVC   DEMKEST,BMEST       CHANGE KEY TO DEST RECORD                    
         MVC   DEMKREF,BREF                                                     
         MVC   0(13,R6),0(R4)      COPY KEY TO IOAREA                           
*                                                                               
         OI    DMINBTS,X'08'       READ FOR ALREADY EXISTING DEST REC           
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         CLC   KEY(13),KEYSAVE     IF RECORD ALREADY EXISTS                     
         BNE   CP20                                                             
*                                                                               
         TM    KEY+13,X'80'        THEN IF IT IS DELETED                        
         BZ    CP10                                                             
         NI    KEY+13,X'7F'        UNDELETE AND WRITE BACK KEY                  
         GOTO1 WRITE                                                            
*                                                                               
CP10     MVC   AIO,AIO2            READ OLD REC INTO VOID                       
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            WRITE BACK NEW ONE                           
         GOTO1 PUTREC                                                           
         B     CP50                                                             
*                                                                               
CP20     GOTO1 ADDREC              ELSE ADD NEW RECORD                          
*                                                                               
CP50     MVI   KEYCHANG,C'Y'       SET DISPLAY ONLY MODE                        
         MVC   0(13,R4),0(R6)      GET CORRECT KEY FROM IOAREA                  
         DROP  R4                                                               
*                                                                               
         XC    OVROPT,OVROPT       CLEAR OUT OPTIONS FIELD                      
         OI    OVROPTH+6,X'80'                                                  
*                                                                               
CPX      B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       CLI   RECFOUND,C'Y'       IF RECORD EXISTS                             
         BNE   VR10                                                             
         MVI   ELCODE,BOKCODEQ                                                  
         GOTO1 REMELEM             THEN REMOVE ELEMENTS                         
         B     VR20                                                             
*                                                                               
VR10     L     R6,AIO              ELSE BUILD RECORD FROM SCRATCH               
         USING DEMRECD,R6                                                       
         XC    0(256,R6),0(R6)                                                  
         MVC   0(13,R6),KEYSAVE                                                 
         MVC   DEMLEN,DATADISP                                                  
         MVC   DEMAGYA,AGENCY                                                   
         DROP  R6                                                               
*                                                                               
VR20     LA    R7,QBOOKS           POINT TO FIRST DEMO BOOK                     
         LA    R2,OVRCEL2H                                                      
         ST    R2,SCRPTR                                                        
         LA    R2,OVRCEL1H         POINT TO FIRST CELL                          
         MVI   ELADDED,C'N'                                                     
*                                                                               
VR30     CLI   0(R7),X'FF'         WHILE NOT END OF DEMO BOOK LIST              
         BE    VR90                                                             
*                                                                               
         LA    R6,ELEM             BUILD DEMO BOOK ELEMENT                      
         USING DEMBOKEL,R6                                                      
         MVI   BOKCODE,BOKCODEQ                                                 
         MVC   BOKBOOK,0(R7)                                                    
         LA    R5,BOKOVLST                                                      
         USING OVLSTD,R5           POINT TO OVERRIDE VALUES LIST                
*                                                                               
         LA    R3,QDEMOS           POINT TO DEMO LIST                           
*                                                                               
VR40     CLI   0(R3),X'FF'         WHILE NOT END OF LIST                        
         BE    VR60                                                             
         TM    1(R2),X'20'         AND NOT END OF LINE                          
         BO    VR60                                                             
*                                                                               
         CLI   5(R2),0             IF CELL NOT EMPTY                            
         BE    VR50                                                             
         ZIC   R4,5(R2)            THEN VALIDATE PERCENTAGE                     
         GOTO1 CASHVAL,DMCB,(1,8(R2)),(R4)                                      
         CLI   0(R1),0                                                          
         BNE   INVERR                                                           
         CLC   4(4,R1),=F'100000'  INVALID IF GREATER THAN 10000                
         BH    INVERR                                                           
         MVC   OVDEMO,0(R3)        ADD OVERRIDE TO LIST                         
         MVC   OVVAL,6(R1)                                                      
         LA    R5,OVLSTL(R5)       BUMP OVERRIDE LIST POINTER                   
         DROP  R5                                                               
*                                                                               
VR50     LA    R3,3(R3)            BUMP TO NEXT DEMO                            
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         B     VR40                END OF WHILE LOOP                            
*                                                                               
VR60     TM    1(R2),X'20'         BUMP R2 TO FIRST CELL OF NEXT LINE           
         BO    VR70                    ON THIS LEVEL                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VR60                                                             
VR70     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   TWOLEVS,C'Y'        IF TWO LEVELS                                
         BNE   VR80                                                             
*                                                                               
         L     R0,SCRPTR           THEN POINT TO OTHER LEVEL AND SAVE           
         ST    R2,SCRPTR               POINTER TO THIS LEVEL                    
         LR    R2,R0                                                            
*                                                                               
         CLI   0(R3),X'FF'         IF MORE DEMOS THEN DO SECOND LEVEL           
         BNE   VR40                                                             
*                                                                               
VR80     SR    R5,R6               COMPUTE ELEMENT LENGTH                       
         C     R5,=F'4'                                                         
         BNH   VR85                IF HIGHER THAN 4                             
         STC   R5,BOKLEN                                                        
         GOTO1 ADDELEM             THEN ADD ELEMENT                             
         MVI   ELADDED,C'Y'                                                     
         DROP  R6                                                               
*                                                                               
VR85     LA    R7,2(R7)            BUMP TO NEXT DEMO BOOK                       
         B     VR30                END OF OUTER WHILE LOOP                      
*                                                                               
VR90     CLI   RECFOUND,C'Y'       IF RECORD EXISTED                            
         BNE   VR95                                                             
         CLI   ELADDED,C'N'        THEN IF RECORD EMPTY                         
         BNE   VR91                                                             
         OI    KEY+13,X'80'        THEN DELETE KEY AND RECORD                   
         GOTO1 WRITE                                                            
         L     R6,AIO                                                           
         OI    15(R6),X'80'                                                     
         B     VR92                                                             
*                                                                               
VR91     TM    KEY+13,X'80'        ELSE IF WAS DELETED                          
         BZ    VR92                                                             
         NI    KEY+13,X'7F'        THEN UNDELETE AND WRITE KEY                  
         GOTO1 WRITE                                                            
         L     R6,AIO              UNDELETE RECORD                              
         NI    15(R6),X'7F'                                                     
*                                                                               
VR92     GOTO1 PUTREC              WRITE RECORD BACK                            
         B     VRX                                                              
*                                                                               
VR95     CLI   ELADDED,C'Y'        ELSE IF ELEMENT WAS ADDED                    
         BNE   VRX                                                              
         GOTO1 ADDREC              THEN ADD RECORD                              
         MVI   RECFOUND,C'Y'       NOW RECORD EXISTS                            
*                                                                               
VRX      B     DR                                                               
         EJECT                                                                  
* DISPLAY DEMO NAMES ACROSS TOP OF VALUE CELLS                                  
*                                                                               
DR       GOTO1 CLEARF,DMCB,(0,OVRL1H),OVRLAST                                   
         GOTO1 CLEARF,DMCB,(1,OVRL1H),OVRLAST                                   
         LA    R2,OVRL1+13                                                      
         LA    R3,QDEMOS           POINT TO DEMO LIST                           
         SR    R4,R4                                                            
*                                                                               
DR10     CLI   0(R3),X'FF'         WHILE NOT END OF DEMO LIST                   
         BE    DR20                                                             
*                                                                               
         LA    R5,DEDBLOCK         DISPLAY NAME OF DEMO                         
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         L     RE,SYSPARMS                                                      
         MVC   DBCOMFCS,16(RE)                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'T'                                                    
         GOTO1 DEMOCON,DMCB,(1,0(R3)),(2,0(R2)),DBLOCK                          
*                                                                               
         LA    R2,9(R2)            BUMP DISPLAY POINTER                         
         LA    R3,3(R3)            BUMP DEMO LIST POINTER                       
         LA    R4,1(R4)                                                         
         C     R4,=F'7'            IF DONE 7 DEMOS                              
         BNE   DR10                                                             
*                                                                               
         LA    R2,OVRL2+13         THEN POINT TO BOTTOM HALF OF DISPLAY         
         B     DR10                END OF WHILE LOOP                            
*                                                                               
DR20     MVI   TWOLEVS,C'N'        TWOLEVS = 'Y' IF TOP AND BOTTOM OF           
         C     R4,=F'7'                DISPLAY ARE USED                         
         BNH   *+8                                                              
         MVI   TWOLEVS,C'Y'                                                     
         EJECT                                                                  
* DISPLAY DEMO BOOK DATES ALONG THE SIDE OF THE CELLS                           
*                                                                               
         LA    R2,OVRBOK1H         POINT TO FIRST DISPLAY LINE                  
         MVI   DOBOTTOM,C'Y'                                                    
*                                                                               
DR30     LA    R7,QBOOKS           POINT TO FIRST DEMO BOOK                     
*                                                                               
DR40     CLI   0(R7),X'FF'         WHILE NOT END OF DEMO BOOK LIST              
         BE    DR60                                                             
         ZIC   R1,1(R7)            DISPLAY BOOK MONTH                           
         BCTR  R1,0                                                             
         M     R0,=F'3'                                                         
         LA    R1,MYMONTHS(R1)                                                  
         MVC   8(3,R2),0(R1)       DISPLAY BOOK YEAR                            
         EDIT  (1,0(R7)),(2,11(R2)),ALIGN=RIGHT,FILL=0                          
*                                                                               
DR50     ZIC   R0,0(R2)            BUMP TO NEXT DISPLAY LINE                    
         AR    R2,R0                                                            
         TM    1(R2),X'20'         SKIP INSIDE CELLS                            
         BZ    DR50                                                             
*                                                                               
         LA    R7,2(R7)            BUMP TO NEXT DEMO BOOK                       
         B     DR40                END OF WHILE LOOP                            
*                                                                               
DR60     CLI   TWOLEVS,C'Y'        IF BOTTOM OF DISPLAY USED                    
         BNE   DR70                                                             
         CLI   DOBOTTOM,C'Y'       AND BOTTOM NOT ALREADY FILLED IN             
         BNE   DR70                                                             
*                                                                               
         LA    R2,OVRBOK2H         THEN DO AGAIN FOR BOTTOM                     
         MVI   DOBOTTOM,C'N'                                                    
         B     DR30                                                             
*                                                                               
DR70     B     DR100                                                            
*                                                                               
MYMONTHS DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
         EJECT                                                                  
* DISPLAY VALUES IN CELLS FOR EACH DEMO BOOK/DEMO                               
*                                                                               
DR100    CLI   RECFOUND,C'Y'       SKIP IF RECORD DOESN'T EXIST                 
         BNE   ENTDATA                                                          
         TM    KEY+13,X'80'        OR IF WAS DELETED                            
         BO    ENTDATA                                                          
*                                                                               
DR110    LA    R7,QBOOKS           POINT TO FIRST DEMO BOOK                     
         LA    R2,OVRCEL2H                                                      
         ST    R2,SCRPTR                                                        
         LA    R2,OVRCEL1H         POINT TO FIRST CELL                          
*                                                                               
DR120    CLI   0(R7),X'FF'         WHILE NOT END OF DEMO BOOK LIST              
         BE    DR190                                                            
         LA    R3,QDEMOS           POINT TO DEMO LIST                           
*                                                                               
DR130    CLI   0(R3),X'FF'         WHILE NOT END OF DEMO LIST                   
         BE    DR150                                                            
         TM    1(R2),X'20'         AND NOT END OF LINE                          
         BO    DR150                                                            
*                                  IF DEMO OVERRIDE EXISTS                      
         GOTO1 FINDOVR,DMCB,(R3),(R7)                                           
         CLC   DMCB+2(2),=X'FFFF'                                               
         BE    DR140                                                            
         L     R8,DMCB             THEN DISPLAY IT                              
         EDIT  (R8),(6,8(R2)),1,ZERO=NOBLANK                                    
*                                                                               
DR140    LA    R3,3(R3)            BUMP TO NEXT DEMO                            
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         B     DR130               END OF WHILE LOOP                            
*                                                                               
DR150    TM    1(R2),X'20'         BUMP R2 TO FIRST CELL OF NEXT LINE           
         BO    DR160                   ON THIS LEVEL                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR150                                                            
DR160    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   TWOLEVS,C'Y'        IF TWO LEVELS                                
         BNE   DR170                                                            
*                                                                               
         L     R0,SCRPTR           THEN POINT TO OTHER LEVEL AND SAVE           
         ST    R2,SCRPTR               POINTER TO THIS LEVEL                    
         LR    R2,R0                                                            
*                                                                               
         CLI   0(R3),X'FF'         IF MORE DEMOS THEN DO SECOND LEVEL           
         BNE   DR130                                                            
*                                                                               
DR170    LA    R7,2(R7)            BUMP TO NEXT DEMO BOOK                       
         B     DR120               END OF WHILE LOOP                            
*                                                                               
DR190    B     EXIT                                                             
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
ERRRNF   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
ENTDATA  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(10),=C'ENTER DATA'                                       
         OI    CONHEADH+6,X'80'                                                 
         OI    CONSERVH+6,X'81'                                                 
         LA    R2,OVRCEL1H                                                      
         GOTO1 ERREX2                                                           
*                                                                               
EXIT     XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(14),=C'DATA DISPLAYED'                                   
         OI    CONHEADH+6,X'80'                                                 
         OI    CONSERVH+6,X'81'                                                 
         LA    R2,OVRCEL1H                                                      
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPCSOFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOF9D                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
KEYCHANG DS    C                   FLAGS IF KEY HAS CHANGED                     
RECFOUND DS    C                   FLAGS IF RECORD EXISTS                       
ELADDED  DS    C                   FLAGS IF ELEMENT EXISTS                      
TWOLEVS  DS    C                   FLAGS IF TWO LEVELS OF DISPLINES             
DOBOTTOM DS    C                   FLAGS IF BOTTOM LEVEL WAS DONE YET           
SCRPTR   DS    A                   PTR TO SCREEN LINE ON OTHER LEVEL            
DEDBLOCK DS    XL256               BLOCK FOR CALLS TO DEMOCON                   
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062SPCSO09   05/01/02'                                      
         END                                                                    

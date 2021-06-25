*          DATA SET RERES25    AT LEVEL 030 AS OF 05/01/02                      
*PHASE T81925A,*                                                                
         TITLE 'T81925 - RERES25 - REP RESEARCH TEXT RECORDS'                   
*                                                                               
**********************************************************************          
*                                                                    *          
*        RERES25 (T81925) --- REP RESEARCH (INVENTORY) TEXT RECORDS  *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* FEB27/91 (MRR) --- MOVED FROM SFM                                  *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*                                                                               
T81925   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1925**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         SPACE                                                                  
         MVI   ACTELOPT,C'N'       DON'T ADD GENCON ACTIVITY ELEMENT            
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,XRECADD        ADD TEXT TO INVENTORY (FORCE)                
         BE    XREC                                                             
         CLI   MODE,XRECPUT        ADD TEXT TO INVENTORY (FORCE)                
         BE    XREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    LIST                                                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
VKEY     DS    0H                                                               
         XC    HDRKEY,HDRKEY                                                    
         XC    STATION(14),STATION                                              
         XC    KFSRC(5),KFSRC                                                   
         NI    STATUS,X'7F'        TURN OFF INV. NUMBER ONLY                    
         LA    R6,HDRKEY                                                        
         USING RINVKEY,R6                                                       
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         SPACE 1                                                                
****************************************************************                
*              VALIDATE STATION (REQUIRED)                     *                
****************************************************************                
         SPACE 1                                                                
         LA    R2,TXTMRKTH         CLEAR OUT MARKET NAME                        
         MVC   8(20,R2),SPACES                                                  
         OI    TXTMRKTH+6,X'80'     TRANSMIT FIELD                              
         SPACE 1                                                                
         LA    R2,TXTSTAH          NOW VALIDATE STATION                         
         GOTO1 ANY                                                              
         GOTO1 VVALSTA                                                          
         MVC   STATION,WORK                                                     
         MVC   MEDIA,WORK+4                                                     
         MVC   RINVKSTA,WORK                                                    
         CLI   WORK+4,C' '         VALISTA RETURNS BLANK FOR                    
         BNE   VK3                 TV AND SATELLITE STATIONS                    
         MVI   RINVKSTA+4,C'T'     KEY NEEDS T FOR TV                           
         MVI   MEDIA,C'T'                                                       
         CLI   WORK+40,C' '        IF SATELLITE, WORK+40=1 OR 2                 
         BE    VK3                                                              
         MVC   RINVKSTA+4(1),WORK+40                                            
         MVC   MEDIA,WORK+40                                                    
         SPACE 1                                                                
VK3      LA    R2,TXTMRKTH                                                      
         MVC   8(20,R2),WORK+10    MARKET NAME                                  
         SPACE 1                                                                
***********************************************************************         
*              VALIDATE TYPE (SOURCE) - OPTIONAL FOR LIST AND REPORT  *         
*    VALID ENTRIES ARE 'MKT'         FOR MARKET TEXT        C'M'      *         
*                OR    'STA'         FOR STATION TEXT       C'S'      *         
*                OR                  FOR INVENTORY TEXT     X'FF'     *         
*                      IIII          WHERE IIII IS AN INVENTORY NUM   *         
*                OR    IIII,MMMDDYY  FOR INVENTORY TEXT AND DATE      *         
***********************************************************************         
         SPACE 1                                                                
         LA    R2,TXTTYPH                                                       
         CLI   ACTNUM,ACTLIST      TYPE NOT REQUIRED FOR LIST                   
         BE    VK5                                                              
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BE    VK5                                                              
         GOTO1 ANY                                                              
         SPACE 1                                                                
VK5      CLI   5(R2),0                                                          
         BNE   VK7                                                              
         XC    SOURCE(17),SOURCE                                                
         B     VK150               ADDITIONAL FIELDS FOR LIST/REPORT            
         SPACE 1                                                                
VK7      CLI   5(R2),3                                                          
         BH    VK20                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,CLCMKT           MARKET FACT                                  
         BE    VK10                                                             
         EX    R1,CLCSTA           STATION FACT                                 
         BNE   VK20                                                             
VK10     MVC   SOURCE,8(R2)                                                     
         B     VK50                                                             
         SPACE 1                                                                
VK20     XC    WORK,WORK                                                        
         MVC   WORK(8),0(R2)       COPY R2 HEADER INTO WORK                     
         CLI   8(R2),C'='          INDICATES LIST ONLY THIS INV NO.             
         BE    VK21                                                             
         ZIC   RE,5(R2)            COPY R2 DATA INTO WORK                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),8(R2)                                                  
         B     VK30                                                             
         SPACE 1                                                                
VK21     CLI   ACTNUM,ACTLIST      PREFIX OF = ONLY OK FOR LIST/REPORT          
         BE    VK22                                                             
         CLI   ACTNUM,ACTREP                                                    
         BE    VK22                                                             
         MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
         SPACE 1                                                                
VK22     OI    STATUS,X'80'                                                     
         ZIC   RE,5(R2)            COPY R2 DATA INTO WORK WITHOUT =             
         BCTR  RE,0                                                             
         STC   RE,WORK+5           ADJUST SIZE OF DATA                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),9(R2)                                                  
         SPACE 1                                                                
VK30     MVI   ERROR,INVALID       INVENTORY/DATE EDIT                          
         MVI   SOURCE,X'FF'        INDICATE RATIONALE RECORD                    
         GOTO1 SCANNER,DMCB,WORK,(2,BLOCK)                                      
         ZIC   R5,DMCB+4           NUMBER OF CHUNKS                             
         LTR   R5,R5                                                            
         BZ    ERREND                                                           
         SPACE 1                                                                
         LA    R4,BLOCK                                                         
         CH    R5,=H'2'                                                         
         BH    ERREND                                                           
         CLI   0(R4),3             INVENTORY NUMBER MUST BE 3                   
         BL    ERREND                                                           
         CLI   0(R4),4             OR 4 CHARACTERS                              
         BH    ERREND                                                           
         CLI   12(R4),C'0'         AND 1ST 2 MUST BE NUMERIC                    
         BL    ERREND                                                           
         CLI   12(R4),C'9'                                                      
         BH    ERREND                                                           
         CLI   13(R4),C'0'                                                      
         BL    ERREND                                                           
         CLI   13(R4),C'9'                                                      
         BH    ERREND                                                           
         SPACE 1                                                                
         PACK  DUB(8),12(2,R4)     QUARTER HOUR CODE                            
         CVB   R0,DUB                                                           
         STC   R0,RINVKQTR                                                      
         MVC   RINVKDAY,14(R4)     DAY CODE                                     
         MVI   RINVKLEN,C'0'       LENGTH                                       
         CLI   0(R4),4                                                          
         BNE   *+10                                                             
         MVC   RINVKLEN,15(R4)                                                  
         MVC   INVNO,RINVKQTR                                                   
         CH    R5,=H'2'                                                         
         BNE   VK50                                                             
         SPACE 1                                                                
         MVI   ERROR,INVDATE                                                    
         LA    R4,32(R4)           NOW VALIDATE DATE                            
         GOTO1 DATVAL,DMCB,(0,12(R4)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,RINVKSTD)                                
         MVC   DATE,RINVKSTD                                                    
         SPACE 1                                                                
VK50     MVC   RINVKSRC,SOURCE                                                  
         SPACE 1                                                                
****************************************************************                
*              VALIDATE TEXT NUMBER (EXCEPT FOR LIST/REPORT)   *                
****************************************************************                
         SPACE 1                                                                
         CLI   ACTNUM,ACTLIST      NO TEXT NUMBER FOR LIST                      
         BE    VK150                                                            
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BE    VK150                                                            
         SPACE 1                                                                
         LA    R2,TXTNUMH                                                       
         MVI   ERROR,NOTNUM                                                     
         TM    4(R2),X'08'         NUMERIC                                      
         BNO   ERREND                                                           
         XC    WORK,WORK                                                        
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,PACK                                                          
         B     VK80                                                             
PACK     PACK  DUB(8),8(0,R2)                                                   
VK80     CVB   R0,DUB                                                           
         CH    R0,=H'9999'                                                      
         BH    ERREND                                                           
         STH   R0,HALF                                                          
         MVC   NUMBER,HALF                                                      
         MVC   RINVKTXT,HALF                                                    
         SPACE 1                                                                
         DROP  R6                                                               
         CLI   SOURCE,X'FF'        DON'T HAVE HEADER FOR MKT/STA                
         BNE   VKXIT                                                            
         SPACE 1                                                                
         MVC   AIO,AIO2            GET HEADER RECORD IN AIO2                    
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         MVC   KEY(24),HDRKEY      REP/STA/INV NO./(START DATE)                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VK90                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         B     VK120                                                            
         SPACE 1                                                                
VK90     MVI   ERROR,NOTFOUND                                                   
         OC    DATE,DATE                                                        
         BNZ   ERREND              INPUT A DATE, RECORD NOT FOUND               
         SPACE 1                                                                
         CLC   KEYSAVE(21),KEY     DID I FIND INVENTORY NUMBER                  
         BNE   ERREND                                                           
         OC    KEY+24(3),KEY+24    MAKE SURE IT'S HEADER                        
         BZ    *+6                                                              
         DC    H'0'                PROBLEM WITH FILE                            
         SPACE 1                                                                
VK100    GOTO1 GETREC              WANT HEADER WITH                             
         L     R6,AIO                                                           
         OC    RINVPEFF+2(2),RINVPEFF+2   NO END DATE                           
         BZ    VK120                                                            
         SPACE 1                                                                
VK110    GOTO1 SEQ                 OR LATEST END DATE                           
         CLC   KEYSAVE(21),KEY                                                  
         BNE   VK120                                                            
         OC    KEY+24(3),KEY+24                                                 
         BZ    VK100               HEADER                                       
         B     VK110               NOT A HEADER                                 
         SPACE 1                                                                
VK120    MVC   KDATE,RINVKSTD      EFFECTIVE START DATE FROM KEY                
         LA    R6,HDRKEY                                                        
         MVC   RINVKSTD,KDATE      SAVE IN HEADER KEY                           
         OC    DATE,DATE           IF DATE WASN'T INPUT, THEN                   
         BNZ   VKXIT                                                            
         LA    R2,TXTTYPH          PUT IT TO SCREEN                             
         LA    R1,8(R2)                                                         
         BAS   RE,INVDISP                                                       
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         B     VKXIT                                                            
         SPACE 1                                                                
****************************************************************                
*              VALIDATE SOURCE FILTER (FOR LIST/REPORT)        *                
****************************************************************                
         SPACE 1                                                                
VK150    LA    R2,LTXKSRCH                                                      
         CLI   5(R2),0                                                          
         BE    VK160                                                            
         MVI   ERROR,INVALID                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KFSRC,8(R2)                                                      
         MVC   WORK(0),8(R2)                                                    
         OC    WORK,SPACES                                                      
         EX    R1,*+12                                                          
         BE    VK160                                                            
         B     *+10                                                             
         CLC   WORK(0),=C'ARB'                                                  
         SPACE 1                                                                
         EX    R1,*+12                                                          
         BE    VK160                                                            
         B     *+10                                                             
         CLC   WORK(0),=C'NSI'                                                  
         SPACE 1                                                                
         EX    R1,*+12                                                          
         BE    VK160                                                            
         B     ERREND                                                           
         CLC   WORK(0),=C'SRC'                                                  
         SPACE 1                                                                
****************************************************************                
*              VALIDATE FILTER BOOK (FOR LIST/REPORT)          *                
****************************************************************                
         SPACE 1                                                                
VK160    LA    R2,LTXKBKH                                                       
         CLI   5(R2),0                                                          
         BE    VK170                                                            
         MVI   BYTE,C'A'                                                        
         CLI   KFSRC,0                                                          
         BE    *+10                                                             
         MVC   BYTE,KFSRC                                                       
         MVI   ERROR,INVALID                                                    
         GOTO1 BOOKVAL,DMCB,(BYTE,(R2)),(1,WORK),SCANNER                        
         CLI   DMCB+4,1                                                         
         BNE   ERREND                                                           
         MVC   KFBK,WORK                                                        
         SPACE 1                                                                
****************************************************************                
*    VALIDATE FILTER FOR LOCAL TEXT ONLY ( FOR LIST/REPORT)    *                
****************************************************************                
         SPACE 1                                                                
VK170    LA    R2,LTXKLOCH                                                      
         CLI   5(R2),0                                                          
         BE    VKXIT                                                            
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'N'                                                       
         BE    VK180                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
VK180    MVC   KFLOC,8(R2)         SAVE FILTER                                  
         SPACE 2                                                                
VKXIT    MVC   AIO,AIO1            BUILD TXT KEY IN AIO1                        
         XC    KEY,KEY                                                          
         MVC   KEY,HDRKEY                                                       
VKXX     B     XIT                                                              
         DROP  R6                                                               
         SPACE 1                                                                
CLCMKT   CLC   8(0,R2),=C'MKT'                                                  
CLCSTA   CLC   8(0,R2),=C'STA'                                                  
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
VREC     DS    0H                                                               
         XC    FSRC(5),FSRC        CLEAR FSRC,FBOOK,INVSRC                      
         MVI   ELCODE,1                                                         
         GOTO1 REMELEM             DELETE OLD X'01' ELEMENT                     
         MVI   ELCODE,2                                                         
         GOTO1 REMELEM             AND OLD X'02' ELEMENT                        
         SPACE 1                                                                
         XC    ELEM,ELEM           AND REBUILD                                  
         LA    R6,ELEM                                                          
         USING RINVFEL,R6                                                       
         MVC   RINVFCOD(2),=X'020A'                                             
         SPACE 1                                                                
****************************************************************                
*              VALIDATE SOURCE                                 *                
****************************************************************                
         SPACE 1                                                                
         LA    R2,TXTSRCH                                                       
         MVI   ERROR,INVALID                                                    
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    VR10                NO SOURCE                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         OC    WORK,SPACES                                                      
         MVC   RINVFSRC,WORK                                                    
         MVC   FSRC,WORK                                                        
         EX    R1,*+12                                                          
         BE    VR10                                                             
         B     *+10                                                             
         CLC   WORK(0),=C'ARB'                                                  
         SPACE 1                                                                
         EX    R1,*+12                                                          
         BE    VR10                                                             
         B     *+10                                                             
         CLC   WORK(0),=C'NSI'                                                  
         SPACE 1                                                                
         EX    R1,*+12                                                          
         BE    VR10                                                             
         B     ERREND                                                           
         CLC   WORK(0),=C'SRC'                                                  
         SPACE 1                                                                
****************************************************************                
*              VALIDATE BOOK                                   *                
****************************************************************                
         SPACE 1                                                                
VR10     LA    R2,TXTBOOKH                                                      
         CLI   5(R2),0                                                          
         BE    VR15                                                             
         MVI   BYTE,C'A'                                                        
         CLI   FSRC,0                                                           
         BE    *+10                                                             
         MVC   BYTE,FSRC                                                        
         GOTO1 BOOKVAL,DMCB,(BYTE,(R2)),(1,WORK),SCANNER                        
         CLI   DMCB+4,1                                                         
         BNE   ERREND                                                           
         MVC   RINVFBK,WORK+1                                                   
         MVC   RINVFBKT,WORK        BOOK SOURCE                                 
         MVC   FBOOK,WORK                                                       
         SPACE 1                                                                
****************************************************************                
*              VALIDATE LOCAL TEXT ONLY -DEFAULT IS NO         *                
****************************************************************                
         SPACE 1                                                                
VR15     LA    R2,TXTLOCH                                                       
         MVI   RINVFLOC,0          SET UP FOR DEFAULT                           
         CLI   5(R2),0                                                          
         BE    VR20                                                             
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'N'          NO IS DEFAULT                                
         BE    VR20                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         MVI   RINVFLOC,C'Y'                                                    
         MVI   KFLOC,C'Y'                                                       
         SPACE 1                                                                
****************************************************************                
*              VALIDATE DEMOS - MAXIMUM 6 ALLOWED              *                
****************************************************************                
         SPACE 1                                                                
VR20     LA    R2,TXTDEMOH                                                      
         CLI   5(R2),0                                                          
         BE    VR50                                                             
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 DEMOVAL,DMCB,(R2),(6,WORK),(0,DBLOCK)                            
         DROP  R4                                                               
         MVI   ERROR,INVALID                                                    
         CLI   DMCB+4,0                                                         
         BE    ERREND                                                           
         ZIC   R4,DMCB+4                                                        
         AH    R4,=H'10'                                                        
         STC   R4,RINVFLEN                                                      
         SH    R4,=H'10'                                                        
         LA    R3,WORK                                                          
         LA    RE,RINVFDEM                                                      
         MVC   0(1,RE),2(R3)       DEMO NUMBERS TO ELEMENT                      
         LA    RE,1(RE)                                                         
         LA    R3,3(R3)                                                         
         BCT   R4,*-14                                                          
         SPACE 1                                                                
VR50     ZIC   R4,RINVFLEN                                                      
         SH    R4,=H'3'                                                         
         EX    R4,*+12                                                          
         BZ    VR90                DON'T ADD ZERO ELEMENT                       
         B     VR60                                                             
         OC    RINVFSRC(0),RINVFSRC                                             
         DROP  R6                                                               
         SPACE 1                                                                
VR60     GOTO1 ADDELEM                                                          
         SPACE 1                                                                
VR90     LA    R2,TXTTEXTH                                                      
         LA    R5,1                LINE NUMBER                                  
         LA    R4,TXTLSTH          END OF SCREEN                                
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND              NO INPUT                                     
         SPACE 1                                                                
VR140    XR    R3,R3                                                            
         XC    ELEM,ELEM           NOW BUILD 01 ELEMENT                         
         LA    R6,ELEM                                                          
         USING RINVTEL,R6                                                       
         MVI   RINVTCOD,X'01'                                                   
         SPACE 1                                                                
         IC    R3,5(R2)                                                         
         LA    R3,6(R3)                                                         
         STC   R3,RINVTLEN         LENGTH                                       
         STC   R5,RINVTLIN         TEXT LINE NUMBER                             
         SH    R3,=H'7'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   RINVTEXT(0),8(R2)   TEXT                                         
         SPACE 1                                                                
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
         LA    R5,1(R5)            NEXT LINE NUMBER                             
         IC    R3,0(R2)                                                         
         AR    R2,R3                                                            
         CR    R2,R4               END OF SCREEN                                
         BE    VR150                                                            
         CLI   5(R2),0             END OF INPUT                                 
         BNE   VR140                                                            
         DROP  R6                                                               
         SPACE 1                                                                
VR150    CLI   ACTNUM,ACTADD                                                    
         BE    VR160                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'EF'        GET ACTIVITY ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   VR160                                                            
         USING RINVAEL,R6                                                       
         MVC   RINVALST,BTODAY                                                  
         MVI   RINVAWHY,C'C'                                                    
         B     VRXIT                                                            
         DROP  R6                                                               
         SPACE 1                                                                
VR160    XC    ELEM,ELEM                                                        
         LA    R6,ELEM             BUILD ACTIVITY ELEMENT                       
         USING RINVAEL,R6                                                       
         MVC   RINVACOD(2),=X'EF0C'                                             
         MVC   RINVAFST,BTODAY                                                  
         MVC   RINVALST,BTODAY                                                  
         MVI   RINVAWHY,C'A'                                                    
         XC    RINVAWHY+1(3),RINVAWHY+1 SPARE                                   
         SPACE 1                                                                
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
         SPACE 1                                                                
VRXIT    B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*    XREC - IF FORCE=Y, ADD TEXT TO INVENTORY THAT MATCHES     *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
XREC     DS    0H                                                               
         SPACE 1                                                                
****************************************************************                
*              VALIDATE FORCE                                  *                
****************************************************************                
         SPACE 1                                                                
         LA    R2,TXTFRCH                                                       
         CLI   5(R2),0                                                          
         BE    XRXIT                                                            
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),1                                                          
         BNE   ERREND                                                           
         CLI   8(R2),C'N'                                                       
         BE    XRXIT                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         CLI   SOURCE,X'FF'                                                     
         BNE   ERREND               CAN'T FORCE STA OR MKT TEXT                 
         SPACE 1                                                                
****************************************************************                
*              FIND MATCHING INVENTORY                         *                
****************************************************************                
         SPACE 1                                                                
         USING RINVKEY,R5                                                       
         L     R5,AIO              TEXT RECORD                                  
         XC    KEY,KEY                                                          
         MVC   KEY(27),RINVKEY                                                  
         MVC   AIO,AIO2            DO INVENTORY STUFF IN IO2                    
         LA    R5,KEY                                                           
         XC    RINVKSRC(3),RINVKSRC                                             
         MVC   RINVKSRC,FSRC       IF FILTER SOURCE, USE IT                     
         GOTO1 HIGH                                                             
         SPACE 1                                                                
XR20     CLC   RINVKEY(RINVKSRC-RINVKEY),KEYSAVE                                
         BNE   XRXIT               CHANGE OF INVENTORY NUMBER                   
         CLI   RINVKSRC,X'FF'                                                   
         BE    XRXIT               STOP AT BEGINNING OF TEXT                    
         CLI   RINVKSRC,0                                                       
         BE    XR100               SKIP THE HEADER                              
         SPACE 1                                                                
         LA    R4,ISRCLIST                                                      
         LA    R3,BITLIST                                                       
S10      CLC   0(1,R4),RINVKSRC                                                 
         BE    S20                                                              
         CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                NOT IN TABLE                                 
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         B     S10                                                              
         SPACE 1                                                                
S20      MVC   INVSRC,0(R3)                                                     
         SPACE 1                                                                
S50      CLI   FSRC,0              IF NO FILTER SOURCE,                         
         BE    S70                 DEFAULT SOURCE FOR FBOOK IS A                
         CLI   FSRC,C'S'                                                        
         BNE   S55                                                              
         TM    INVSRC,X'41'                                                     
         BO    S70                 OK SO FAR                                    
         B     XR100               SKIP IT                                      
S55      CLI   FSRC,C'N'                                                        
         BNE   S60                                                              
         TM    INVSRC,X'41'                                                     
         BO    XR100               SKIP IT                                      
         TM    INVSRC,X'40'                                                     
         BZ    XR100               SKIP IT                                      
         B     S70                 OK SO FAR                                    
         SPACE 1                                                                
S60      TM    INVSRC,X'40'                                                     
         BO    XR100               SKIP IT                                      
         SPACE 1                                                                
S70      OC    FBOOK,FBOOK                                                      
         BZ    XR39                                                             
         CLC   RINVKBK,FBOOK+1                                                  
         BNE   XR100               NOT THE SAME YEAR/MONTH                      
         SPACE 1                                                                
         CLC   INVSRC,FBOOK                                                     
         BE    XR39                                                             
         CLI   FSRC,0                                                           
         BNE   XR100                                                            
         NI    INVSRC,X'BF'                                                     
         CLC   INVSRC,FBOOK                                                     
         BE    XR39                                                             
         BNE   XR100                                                            
         SPACE 1                                                                
****************************************************************                
*              ADD TEXT TO THIS INVENTORY                      *                
****************************************************************                
         SPACE 1                                                                
XR39     GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
         BNE   XR40                                                             
         USING RINVCEL,R6                                                       
         CLC   RINVCTXT,NUMBER                                                  
         BE    XR100               ALREADY THERE                                
         MVC   RINVCTXT,NUMBER                                                  
         B     XR50                                                             
         SPACE 1                                                                
XR40     XC    ELEM,ELEM           NO CD ELEMENT - ADD ONE                      
         LA    R6,ELEM                                                          
         MVC   RINVCCOD(2),=X'CD0A'                                             
         MVC   RINVCTXT,NUMBER                                                  
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
XR50     GOTO1 PUTREC                                                           
         SPACE 1                                                                
XR100    GOTO1 SEQ                 NEXT INVENTORY RECORD                        
         B     XR20                                                             
         SPACE 1                                                                
XRXIT    MVC   AIO,AIO1            RESTORE IO AREA                              
         B     XIT                                                              
         SPACE 3                                                                
ISRCLIST DC    C'ABCDE'            ARB                                          
         DC    C'NOPQR'            NSI                                          
         DC    C'TUX'              SRC                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
BITLIST  DC    X'0004080220'       ARB  ( PTSE)                                 
         DC    X'4044484260'       NSI  ( PTSE)                                 
         DC    X'414561'           SRC  ( PE)                                   
         DC    X'FF'                                                            
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE 2                                                                
DKEY     DS    0H                                                               
         MVC   SVKEY,KEY           SAVE KEY AND                                 
         MVC   SVDMWORK,DMWORK+4   SAVE D/A (LATER GETREC IN VALISTA)           
         L     R6,AIO              RECORD SELECTED                              
         USING RINVKEY,R6                                                       
         SPACE 1                                                                
         LA    R2,TXTSTAH          STATION                                      
         LA    RE,8(R2)                                                         
         MVC   0(4,RE),RINVKSTA                                                 
         LA    RE,3(RE)                                                         
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         CLI   RINVKSTA+4,C'T'                                                  
         BE    DK20                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),RINVKSTA+4                                               
DK20     OI    TXTSTAH+6,X'80'     TRANSMIT FIELD                               
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 VVALSTA             GET MARKET NAME                              
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         LA    R2,TXTMRKTH         MARKET NAME                                  
         MVC   8(20,R2),WORK+10                                                 
         OI    TXTMRKTH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,TXTTYPH                                                       
         MVC   SOURCE,RINVKSRC                                                  
         CLI   RINVKSRC,X'FF'                                                   
         BE    DK30                                                             
         MVC   8(3,R2),=C'MKT'                                                  
         CLI   RINVKSRC,C'M'                                                    
         BE    DK40                                                             
         MVC   8(3,R2),=C'STA'                                                  
         B     DK40                                                             
DK30     LA    R1,8(R2)            OR                                           
         MVC   INVNO,RINVKINV                                                   
         MVC   KDATE,RINVKSTD                                                   
         MVC   DATE,RINVKSTD                                                    
         BAS   RE,INVDISP          INVENTORY NUMBER, DATE                       
         SPACE 1                                                                
DK40     OI    TXTTYPH+6,X'80'     TRANSMIT FIELD                               
         SPACE 1                                                                
         LA    R2,TXTNUMH          TEXT NUMBER                                  
         MVC   NUMBER,RINVKTXT                                                  
         EDIT  (2,RINVKTXT),(5,8(R2)),ALIGN=LEFT                                
         OI    TXTNUMH+6,X'80'     TRANSMIT FIELD                               
         SPACE 1                                                                
DKXIT    DS    0H           NEED TO DO ANOTHER GETREC BEFORE PUTREC             
*                             (DID GETREC IN VALISTA FOR MKT NAME)              
         MVC   AIO,AIO2            PUT IT IN AIO2                               
         MVC   KEY(27),SVKEY                                                    
         MVC   KEY+28(4),SVDMWORK                                               
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            NEW RECORD IS IN AIO1                        
DKXX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DREC     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RINVTEL,R6                                                       
         SPACE 1                                                                
         LA    R2,TXTSRCH          CLEAR OUT SOURCE                             
         BAS   RE,CLEAR                                                         
         LA    R2,TXTBOOKH         BOOK                                         
         BAS   RE,CLEAR                                                         
         LA    R2,TXTLOCH          LOCAL TEXT ONLY                              
         BAS   RE,CLEAR                                                         
         LA    R2,TXTDEMOH         AND DEMO FIELDS                              
         BAS   RE,CLEAR                                                         
         SPACE 1                                                                
         LA    R2,TXTTEXTH         FIRST TEXT FIELD                             
         LA    R4,TXTLSTH          LAST FIELD ON SCREEN                         
         SR    R3,R3                                                            
DR5      CR    R2,R4                                                            
         BE    DR8                                                              
         BAS   RE,CLEAR            CLEAR ALL TEXT FIELDS                        
         IC    R3,0(R2)                                                         
         AR    R2,R3               NEXT FIELD                                   
         B     DR5                                                              
         SPACE 1                                                                
DR8      LA    R2,TXTTEXTH         TEXT                                         
DR10     XR    R3,R3                                                            
         IC    R3,RINVTLEN         ELEMENT LENGTH                               
         SH    R3,=H'7'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),RINVTEXT                                                 
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         IC    R3,0(R2)                                                         
         AR    R2,R3               NEXT FIELD                                   
         IC    R3,1(R6)            NEXT ELEMENT                                 
         AR    R6,R3                                                            
         CLI   0(R6),1                                                          
         BE    DR10                                                             
         DROP  R6                                                               
         SPACE 1                                                                
         LA    R2,TXTLOCH          PRESET LOCAL TEXT TO NO                      
         MVI   8(R2),C'N'                                                       
         SPACE 1                                                                
         L     R6,AIO              IS THERE A FILTER ELEMENT?                   
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRXIT                                                            
         USING RINVFEL,R6                                                       
         CLI   RINVFSRC,0                                                       
         BE    DR20                                                             
         LA    R2,TXTSRCH                                                       
         CLI   RINVFSRC,C'A'                                                    
         BNE   *+10                                                             
         MVC   8(3,R2),=C'ARB'                                                  
         CLI   RINVFSRC,C'N'                                                    
         BNE   *+10                                                             
         MVC   8(3,R2),=C'NSI'                                                  
         CLI   RINVFSRC,C'S'                                                    
         BNE   *+10                                                             
         MVC   8(3,R2),=C'SRC'                                                  
         SPACE 1                                                                
DR20     OC    RINVFBK,RINVFBK                                                  
         BZ    DR25                                                             
         LA    R2,TXTBOOKH                                                      
         MVC   WORK(1),RINVFBKT                                                 
         MVC   WORK+1(2),RINVFBK                                                
         BAS   RE,FMTBOOK                                                       
         MVC   8(6,R2),WORK+10                                                  
         SPACE 1                                                                
DR25     LA    R2,TXTLOCH          LOCAL TEXT ONLY?                             
         CLI   RINVFLOC,C'Y'                                                    
         BNE   DR30                                                             
         MVI   8(R2),C'Y'                                                       
         SPACE 1                                                                
DR30     ZIC   R3,RINVFLEN                                                      
         SH    R3,=H'10'                                                        
         BNP   DRXIT                                                            
         STC   R3,BYTE             R3 AND BYTE HAVE NUMBER OF DEMOS             
         LA    R2,TXTDEMOH                                                      
         LA    RE,RINVFDEM                                                      
         XC    WORK,WORK           WORK WILL HAVE FULL DEMO EXPRESSIONS         
         LA    R4,WORK                                                          
DR50     MVI   1(R4),C'T'                                                       
         MVC   2(1,R4),0(RE)                                                    
         LA    R4,3(R4)                                                         
         LA    RE,1(RE)            POINT TO NEXT DEMO                           
         BCT   R3,DR50             ANY MORE                                     
         SPACE 1                                                                
         LA    R4,BLOCK                                                         
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 DEMOCON,DMCB,(BYTE,WORK),(9,8(R2)),(0,DBLOCK)                    
         DROP  R4                                                               
DRXIT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*                        LIST AND PRINT ROUTINE                *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
LIST     DS    0H                                                               
         CLI   MODE,PRINTREP                                                    
         BNE   LR5                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         LA    R1,BUFF             CLEAR BUFF TO SPACES                         
         LA    RE,45                                                            
LR3      MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   RE,LR3                                                           
         SPACE 1                                                                
LR5      LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
         OC    KEY(27),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,AGENCY                                                  
         MVC   RINVKSTA,STATION                                                 
         CLI   SOURCE,0                                                         
         BE    LR7                                                              
         MVC   RINVKSRC,SOURCE                                                  
         OC    INVNO,INVNO                                                      
         BZ    LR7                                                              
         MVC   RINVKINV,INVNO                                                   
         OC    DATE,DATE                                                        
         BZ    LR7                                                              
         MVC   RINVKSTD,DATE                                                    
LR7      MVC   SAVEKEY,KEY                                                      
LR10     GOTO1 HIGH                                                             
LR15     CLC   KEY(17),SAVEKEY     CORRECT REP AND STATION                      
         BNE   LRXIT                                                            
         CLI   RINVKSRC,C'M'       ONLY WANT TEXT - MARKET                      
         BE    LR20                                                             
         CLI   RINVKSRC,C'S'       STATION                                      
         BE    LR20                                                             
         CLI   RINVKSRC,X'FF'      OR INVENTORY TEXT                            
         BNE   LR200                                                            
         SPACE 1                                                                
LR20     CLI   SOURCE,0                                                         
         BE    LR30                                                             
         CLC   RINVKSRC(1),SOURCE                                               
         BNE   LRXIT                                                            
         CLI   SOURCE,X'FF'                                                     
         BNE   LR30                                                             
         TM    STATUS,X'80'        LIST THIS INVENTORY NUMBER ONLY              
         BZ    LR30                                                             
         CLC   RINVKINV(3),INVNO                                                
         BNE   LRXIT                                                            
         OC    DATE,DATE                                                        
         BZ    LR30                                                             
         CLC   RINVKSTD(3),DATE                                                 
         BNE   LRXIT                                                            
         SPACE 1                                                                
LR30     MVC   LISTAR,SPACES       CLEAR OUT LIST LINE                          
         LA    R2,LISTAR                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR35                                                             
         LA    R2,P                OR USE P IF HARDCOPY                         
         MVC   P,SPACES                                                         
         USING LISTD,R2                                                         
LR35     GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        TEXT FILTER ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    LR40                                                             
         CLI   KFLOC,C'Y'          IF THEY WANT ONLY LOCAL TEXT                 
         BE    LR200               THEY DON'T WANT THIS ONE                     
         MVI   LFLOC,C'N'                                                       
         B     LR100               ELSE, INCLUDE THIS TEXT                      
         SPACE 1                                                                
         USING RINVFEL,R6                                                       
LR40     OC    KFSRC,KFSRC         SOURCE FILTER                                
         BZ    LR45                                                             
         CLC   KFSRC,RINVFSRC       LIST MATCHING SOURCE                        
         BE    LR45                                                             
         CLI   RINVFSRC,0          AND NO SOURCE                                
         BNE   LR200                                                            
         SPACE 1                                                                
*                                  BOOK FILTER                                  
LR45     OC    KFBK,KFBK                                                        
         BZ    LR50                   NO SCREEN FILTERS                         
         OC    RINVFBK,RINVFBK                                                  
         BZ    LR50                   NO TEXT FILTERS                           
         SPACE 1                                                                
         CLC   KFBK+1(2),RINVFBK   DOES YEAR/MONTH MATCH                        
         BNE   LR200                                                            
         MVC   BYTE,KFBK                                                        
         CLC   BYTE,RINVFBKT       DOES BOOK TYPE MATCH                         
         BE    LR50                YES                                          
         CLI   KFSRC,0             NO, BUT NO SOURCE ASSUMES ARB                
         BNE   LR200                                                            
         OI    BYTE,X'40'          SO CHECK NSI ALSO                            
         CLC   BYTE,RINVFBKT                                                    
         BE    LR50                YES                                          
         OI    BYTE,X'01'          AND CHECK SRC ALSO                           
         CLC   BYTE,RINVFBKT                                                    
         BNE   LR200                                                            
         SPACE 1                                                                
LR50     OC    KFLOC,KFLOC         LOCAL TEXT ONLY FILTER                       
         BZ    LR60                                                             
         CLI   KFLOC,C'Y'          Y=ONLY LOCAL, N=EXLCLUDE LOCAL               
         BE    LR55                                                             
         CLI   RINVFLOC,0                                                       
         BE    LR60                                                             
         B     LR200                                                            
LR55     CLI   RINVFLOC,C'Y'                                                    
         BNE   LR200                                                            
         SPACE 1                                                                
LR60     MVC   LFSRC,RINVFSRC                                                   
         OC    RINVFBK,RINVFBK                                                  
         BZ    LR65                                                             
         MVC   WORK(1),RINVFBKT                                                 
         MVC   WORK+1(2),RINVFBK                                                
         BAS   RE,FMTBOOK                                                       
         MVC   LFBOOK,WORK+10                                                   
         SPACE 1                                                                
LR65     MVI   LFLOC,C'N'                                                       
         CLI   RINVFLOC,C'Y'                                                    
         BNE   *+8                                                              
         MVI   LFLOC,C'Y'                                                       
         SPACE 1                                                                
         ZIC   R3,RINVFLEN                                                      
         SH    R3,=H'10'                                                        
         BNP   LR100                                                            
         CLI   MODE,PRINTREP       6 DEMOS FIT ON PRINT LINE,                   
         BE    LR70                                                             
         CH    R3,=H'5'            BUT ONLY 5 DEMOS CAN FIT ON SCREEN           
         BNH   LR70                SO USE THE SMALLER                           
         LA    R3,5                                                             
         MVI   LFDEMOX,C'*'        INDICATES MORE DEMOS THAN CAN FIT            
         SPACE 1                                                                
LR70     STC   R3,BYTE             R3 AND BYTE HAVE NUMBER OF DEMOS             
         LA    RE,RINVFDEM                                                      
         XC    WORK,WORK           WORK WILL HAVE FULL DEMO EXPRESSIONS         
         LA    R5,WORK                                                          
LR80     MVI   1(R5),C'T'                                                       
         MVC   2(1,R5),0(RE)                                                    
         LA    R5,3(R5)                                                         
         LA    RE,1(RE)            POINT TO NEXT DEMO                           
         BCT   R3,LR80             ANY MORE                                     
         DROP  R6                                                               
         SPACE 1                                                                
         LA    R4,BLOCK                                                         
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 DEMOCON,DMCB,(BYTE,WORK),(9,LFDEMO),(0,DBLOCK)                   
         DROP  R4                                                               
         SPACE 1                                                                
LR100    L     R6,AIO                                                           
         USING RINVKEY,R6                                                       
         CLI   RINVKSRC,X'FF'                                                   
         BE    *+14                                                             
         MVC   LTYPE(1),RINVKSRC                                                
         B     LR105                                                            
         SPACE 1                                                                
         LA    R1,LTYPE                                                         
         MVC   INVNO,RINVKINV                                                   
         MVC   KDATE,RINVKSTD                                                   
         BAS   RE,INVDISP                                                       
         SPACE 1                                                                
LR105    EDIT  (2,RINVKTXT),(5,LTXTNO)                                          
         DROP  R6                                                               
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LR150                                                            
         LA    R4,BUFF                                                          
         SR    R5,R5               LINE COUNTER                                 
         MVC   18(114,R4),P       FOR REPORT-PRINT LIST STUFF                   
         LA    R4,132(R4)                                                       
         MVC   0(132,R4),SPACES                                                 
         LA    R5,2(R5)                                                         
         LA    R4,132(R4)                                                       
         SPACE 1                                                                
         L     R6,AIO              PLUS THE TEXT                                
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RINVTEL,R6                                                       
         SPACE 1                                                                
         XR    R3,R3                                                            
LR110    IC    R3,RINVTLEN         ELEMENT LENGTH                               
         SH    R3,=H'7'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   23(0,R4),RINVTEXT                                                
         LA    R5,1(R5)                                                         
         LA    R4,132(R4)                                                       
         IC    R3,1(R6)            NEXT ELEMENT                                 
         AR    R6,R3                                                            
         CLI   0(R6),1                                                          
         BE    LR110                                                            
         MVC   0(132,R4),SPACES    2 SPACING LINES BETWEEN TEXTS                
         LA    R4,132(R4)                                                       
         MVC   0(132,R4),SPACES                                                 
         LA    R5,2(R5)                                                         
         SPACE                                                                  
         STC   R5,ALLOWLIN         MAKE SURE TEXT FITS ON 1 PAGE                
         LA    R4,BUFF             AND PRINT IT                                 
LR130    MVC   P,0(R4)                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   0(132,R4),SPACES    RECLEAR BUFF TO SPACES                       
         LA    R4,132(R4)                                                       
         BCT   R5,LR130                                                         
         B     LR200                                                            
         DROP  R6                                                               
         SPACE 1                                                                
LR150    GOTO1 LISTMON             FOR LIST                                     
         SPACE 1                                                                
LR200    GOTO1 SEQ                 NEXT RECORD                                  
         LA    R6,KEY                                                           
         B     LR15                                                             
         SPACE 1                                                                
LRXIT    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
** ROUTINE TO DISPLAY INVENTORY NUMBER AND DATE                                 
*  ON ENTRY, R1 POINTS TO OUTPUT AREA                                           
*            INVNO HAS INVENTORY NUMBER                                         
*            KDATE HAS DATE                                                     
         SPACE 1                                                                
INVDISP  NTR1                                                                   
         LR    R3,R1                                                            
         SR    RE,RE                                                            
         IC    RE,INVNO                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   0(2,R3),DUB+1       QTR HOUR                                     
         IC    RE,INVNO+1                                                       
         STC   RE,2(R3)                                                         
         LA    R3,3(R3)                                                         
         CLI   INVNO+2,C'0'                                                     
         BE    *+14                                                             
         MVC   0(1,R3),INVNO+2                                                  
         LA    R3,1(R3)                                                         
         MVI   0(R3),C','          DATE                                         
         GOTO1 DATCON,DMCB,(3,KDATE),(8,1(R3))                                  
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE CLEARS OUT A FIELD                                               
* ON ENTRY, R2 POINTS TO FIELD HEADER                                           
         SPACE 1                                                                
CLEAR    DS    0H                                                               
         ZIC   R1,0(R2)            GET LENGTH OF FIELD                          
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'         EXTENDED HEADER                              
         BZ    CL10                                                             
         SH    R1,=H'8'                                                         
         SPACE 1                                                                
CL10     EX    R1,CLEAREX                                                       
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         BR    RE                                                               
         SPACE 2                                                                
CLEAREX  XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
*  THIS ROUTINE FORMATS THE BOOK TYPE, MONTH AND YEAR                           
*  ON ENTRY, WORK HAS BOOKTYPE                                                  
*            WORK+1 HAS YEAR                                                    
*            WORK+2 HAS MONTH                                                   
*  ON EXIT, WORK+10(6) HAS OUTPUT IE PJUL87                                     
         SPACE 2                                                                
FMTBOOK  NTR1                                                                   
         MVC   WORK+10(6),SPACES                                                
         LA    RE,WORK+10          FIRST DO BOOKTYPE, IF THERE IS ONE           
         SPACE 1                                                                
         LA    R1,SVCLST      CONVERT FROM BOOKVAL TO PRINTABLE PREFIX          
FMT10    CLC   WORK(1),3(R1)                                                    
         BE    FMT20                                                            
         LA    R1,L'SVCLST(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   FMT10                                                            
         DC    H'0'                                                             
FMT20    CLI   1(R1),C' '                                                       
         BE    FMT50                                                            
         MVC   0(1,RE),1(R1)                                                    
         LA    RE,1(RE)                                                         
         SPACE 1                                                                
FMT50    SR    R1,R1                                                            
         IC    R1,WORK+2           NOW DO MONTH                                 
         MH    R1,=H'3'                                                         
         LA    R1,MONTH(R1)                                                     
         MVC   0(3,RE),0(R1)                                                    
         LA    RE,3(RE)                                                         
         SPACE 1                                                                
         SR    R1,R1                                                            
         IC    R1,WORK+1           AND YEAR                                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   0(2,RE),DUB+1                                                    
         B     XIT                                                              
         SPACE 2                                                                
MONTH    DC    C'ESTJANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                       
         EJECT                                                                  
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
****************************************************************                
*  HEDSPECS                                                    *                
****************************************************************                
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H2,1,AGYNAME                                                     
         SSPEC H3,1,C'STATION -'                                                
         SSPEC H1,52,C'TEXT LISTING'                                            
         SSPEC H2,52,C'------------'                                            
         SSPEC H1,93,RUN                                                        
         SSPEC H2,93,REPORT                                                     
         SSPEC H2,109,PAGE                                                      
         DC    X'00'                                                            
         SPACE 4                                                                
HOOK     NTR1                                                                   
         LA    R2,TXTSTAH                                                       
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   H3+11(0),8(R2)      STATION                                      
         MVC   H3+20(20),TXTMRKT   MARKET NAME                                  
         SPACE 1                                                                
         LA    R2,TXTTYPH          INVENTORY NUMBER IS OPTIONAL FILTER          
         CLI   5(R2),0                                                          
         BE    HK50                                                             
         MVC   H3+92(6),=C'TYPE -'                                              
         CLI   8(R2),C'='                                                       
         BNE   HK40                                                             
         ZIC   RE,5(R2)                                                         
         SH    RE,=H'2'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   H3+99(0),9(R2)                                                   
         B     HK50                                                             
         SPACE 1                                                                
HK40     MVC   H3+99(13),=C'STARTING WITH'                                      
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   H3+113(0),8(R2)                                                  
         SPACE 1                                                                
HK50     LA    R2,LTXKSRCH          SOURCE IS OPTIONAL FILTER                   
         LA    R3,H4+92                                                         
         CLI   5(R2),0                                                          
         BE    HK60                                                             
         MVC   0(8,R3),=C'SOURCE -'                                             
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   9(0,R3),8(R2)                                                    
         LA    R3,H4+108                                                        
         SPACE 1                                                                
HK60     LA    R2,LTXKBKH          BOOK IS OPTIONAL FILTER                      
         CLI   5(R2),0                                                          
         BE    HK100                                                            
         MVC   0(6,R3),=C'BOOK -'                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   7(0,R3),8(R2)                                                    
         SPACE 1                                                                
HK100    LA    R2,LTXKLOCH         LOCAL TEXT ONLY IS OPTIONAL FILTER           
         MVC   H4(20),SPACES                                                    
         CLI   5(R2),0                                                          
         BE    HK150                                                            
         MVC   H4(20),=CL20'EXCLUDING LOCAL TEXT'                               
         CLI   8(R2),C'N'                                                       
         BE    HK150                                                            
         MVC   H4(20),=CL20'LOCAL TEXT ONLY'                                    
         SPACE 1                                                                
HK150    LA    R3,H6               FILTER HEADLINES                             
         MVC   18(42,R3),LTXHDS                                                 
         LA    R3,H7                                                            
         MVC   18(77,R3),DASHL                                                  
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
ERREND   GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
DASHL    DC    132C'-'                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
LISTD    DSECT                                                                  
LTYPE    DS    CL13                                                             
         DS    CL1                                                              
LTXTNO   DS    CL5                                                              
         DS    CL3                                                              
LFSRC    DS    CL1                                                              
         DS    CL2                                                              
LFBOOK   DS    CL6                                                              
         DS    CL1                                                              
LFLOC    DS    CL1                                                              
         DS    CL1                                                              
LFDEMO   DS    CL36                                                             
LFDEMOX  DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERESD7D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RERESD8D                                                       
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   OVWORK                                                           
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
SAVEKEY  DS    CL27                                                             
SVKEY    DS    CL27                FOR INTERVENING GETRECS                      
HDRKEY   DS    CL27                FOR INVENTORY HEADER                         
SVDMWORK DS    F                                                                
STATION  DS    CL4                 STATION CALL LETTERS                         
MEDIA    DS    CL1                   MEDIA - TV=BLANK                           
SOURCE   DS    CL1                 M=MARKET, S=STATION, X'FF'=INV. NO           
NUMBER   DS    CL2                 TEXT NUMBER                                  
INVNO    DS    CL3                 INVENTORY NUMBER                             
DATE     DS    XL3                 EFFECTIVE DATE-INPUT                         
FSRC     DS    CL1                 FILTER SOURCE FROM RECORD                    
FBOOK    DS    CL3                 FILTER BOOK FROM RECORD                      
INVSRC   DS    CL1                 INVENTORY SOURCE                             
KDATE    DS    XL3                 EFFECTIVE DATE-FROM HEADER                   
KFSRC    DS    CL1                 FILTER SOURCE FOR LIST/REPORT                
KFBK     DS    CL3                 FILTER BOOK FOR LIST/REPORT                  
KFLOC    DS    CL1                 FLTR LOCAL TEXT FOR LIST/RPT (0,Y,N)         
STATUS   DS    XL1                 X'80' LIST INVENTORY NUMBER ONLY             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030RERES25   05/01/02'                                      
         END                                                                    

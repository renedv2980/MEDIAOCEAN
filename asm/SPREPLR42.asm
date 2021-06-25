*          DATA SET SPREPLR42  AT LEVEL 011 AS OF 05/01/02                      
*PHASE SPLR02H                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE SORTER                                                                 
SPLR02H  TITLE 'SPREPLR42 : CREATE SPECIAL RECDS FOR NSI WEEKLY TP'             
***********************************************************************         
*================================ MAIN ===============================*         
SPLR42   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,SPLR42,R9,RR=R5,CLEAR=YES                            
         LR    R8,RC                                                            
         USING WORKD,R8                                                         
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
                                                                                
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         EJECT                                                                  
*                                                                               
*--------------------------- INITIALIZATION --------------------------*         
*                                                                               
** ADCONS **                                                                    
*                                                                               
         DS    0H                                                               
         L     R2,=A(DISPTAB-SPLR42)                                            
         LA    R2,SPLR42(R2)                                                    
         LA    R0,DISPTABQ                                                      
                                                                                
MI10     DS    0H                                                               
         LA    RE,SPLR42           RE = BASE OF TABLE/ROUTINE                   
         CLI   0(R2),C'C'                                                       
         BE    *+12                                                             
         LA    RE,WORKD                                                         
         CLI   0(R2),C'W'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZICM  R1,1(R2),(3)        R1 = DISPL OF TABLE/ROUTINE                  
         AR    R1,RE                                                            
         ZICM  RF,3(R2),(3)                                                     
         LA    RF,WORKD(RF)        RF-->PLACE TO STORE ADDRESS                  
         ST    R1,0(RF)                                                         
         LA    R2,L'DISPTAB(R2)                                                 
         BCT   R0,MI10                                                          
                                                                                
*                                                                               
** FILTER VALUES **                                                             
*                                                                               
         DS    0H                  STATION CALL LETTERS                         
         CLC   QSTA(3),=C'ALL'                                                  
         BNE   *+10                                                             
         MVC   QSTA(3),=C'     '                                                
         MVC   FLTSTTN,QSTA         (MAY BE BLANKS)                             
                                                                                
*                                                                               
         DS    0H                  YEAR FOR BOOK                                
         MVI   FLTBKY,0             ASSUME NONE                                 
         CLC   QBOOK1(2),SPACES                                                 
         BE    SPL019                                                           
         PACK  DUB,QBOOK1(2)                                                    
         CVB   R0,DUB                                                           
         STC   R0,FLTBKY                                                        
         XI    FLTBKY,X'FF'                                                     
SPL019   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  RATING SERVICE MARKET                        
         XC    FLTRMKT,FLTRMKT      ASSUME NONE                                 
         CLC   QMKT,=C'MLST'        SEE IF HARD-CODED MARKET LIST               
         BNE   *+14                                                             
         MVC   FLTRMKT,=C'ML'        YES, SET SPECIAL CODE                      
         B     SPL029                                                           
                                                                                
*&&DO                                                                           
         CLC   QMKT(3),=C'ALL'                                                  
         BNE   *+10                                                             
         MVC   QMKT,SPACES                                                      
         CLC   QMKT,SPACES                                                      
*&&                                                                             
         CLC   QMKT,SPACES                                                      
         BNE   *+10                                                             
         MVC   QMKT(3),=C'ALL'                                                  
         CLC   QMKT(3),=C'ALL'                                                  
         BE    SPL029                                                           
         PACK  DUB,QMKT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,FLTRMKT                                                     
SPL029   EQU   *                                                                
                                                                                
*                                                                               
** OTHER STUFF **                                                               
*                                                                               
         DS    0H                                                               
         XC    DIRMAX,DIRMAX                                                    
         PACK  DUB,QUESTOR+4(8)    COLUMN 73                                    
         CVB   R0,DUB                                                           
         ST    R0,DIRMAX           MAX NUMBER OF DIRECTORY READS                
                                                                                
*                                                                               
         DS    0H                                                               
         XC    THISSTTN,THISSTTN                                                
         XC    SPLKEY,SPLKEY                                                    
         MVC   SPLKEY(3),=C'RWN'                                                
                                                                                
*                                                                               
** OPEN SORTER **                                                               
*                                                                               
         DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         EJECT                                                                  
*                                                                               
*---------------------------- FILE READS ----------------------------*          
*                                                                               
** READ DIRECTORY RECORDS **                                                    
*                                                                               
SPLD10   DS    0H                                                               
*^^TEST                                                                         
         OC    DIRMAX,DIRMAX                                                    
         BZ    *+14                                                             
         CLC   DIRTTL,DIRMAX                                                    
         BNL   SPLPUT                                                           
*^^EOTEST                                                                       
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,READHI,DEMDIR,SVSPLKEY,SPLKEY                       
         B     SPLD10B                                                          
*                                                                               
SPLD10A  DS    0H                                                               
*^^TEST                                                                         
         OC    DIRMAX,DIRMAX                                                    
         BZ    *+14                                                             
         CLC   DIRTTL,DIRMAX                                                    
         BNL   SPLPUT                                                           
*^^EOTEST                                                                       
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,READSQ,DEMDIR,SVSPLKEY,SPLKEY                       
*                                                                               
SPLD10B  DS    0H                                                               
         CLI   8(R1),0                                                          
         BNE   SPLPUT                                                           
         CLC   SVSPLKEY(3),SPLKEY  DO 'RWN'S ONLY                               
         BNE   SPLPUT                                                           
*                                                                               
         LA    R1,DIRTTL                                                        
         BAS   RE,COUNTER                                                       
                                                                                
*                                                                               
*** FILTER LOGIC ***                                                            
*                                                                               
         DS    0H                                                               
         LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
                                                                                
*                                                                               
         DS    0H                  SKIP SPECIAL RECORDS ALREADY CREATED         
         CLI   DRBOOK+1,X'FF'                                                   
         BE    SPLD10A                                                          
                                                                                
*                                                                               
         DS    0H                  FILTER ON STATION                            
         CLC   FLTSTTN,SPACES                                                   
         BE    SPLD18X                                                          
         CLC   DRSTAT,FLTSTTN                                                   
         BE    SPLD18X                                                          
         MVI   DRSTAT,X'FF'                                                     
         MVC   DRSTAT+1(L'DRKMAJOR-(DRSTAT+1-DRKEY)),DRSTAT                     
         BH    SPLD10                                                           
         MVC   DRSTAT,FLTSTTN                                                   
         XC    DRSTAT+L'DRSTAT(L'DRKMAJOR-(DRSTAT+L'DRSTAT-DRKEY)),DRST+        
               AT+L'DRSTAT                                                      
         B     SPLD10                                                           
SPLD18X  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  FILTER ON BOOK YEAR                          
         CLI   FLTBKY,0                                                         
         BE    SPLD22X                                                          
         CLC   DRBOOK+0(1),FLTBKY                                               
         BE    SPLD22X                                                          
         MVI   DRBOOK,X'FF'                                                     
         MVC   DRBOOK+1(L'DRKMAJOR-(DRBOOK+1-DRKEY)),DRBOOK                     
         BH    SPLD10                                                           
         MVC   DRBOOK(1),FLTBKY                                                 
         XC    DRBOOK+1(L'DRKMAJOR-(DRBOOK+1-DRKEY)),DRBOOK+1                   
         B     SPLD10                                                           
SPLD22X  EQU   *                                                                
         DROP  R6                                                               
                                                                                
*                                                                               
*** PROCESS DIRECTORY RECORD ***                                                
*                                                                               
SPLD20   DS    0H                                                               
         LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
         MVC   NDXDA,DRNDXDA                                                    
                                                                                
         L     RF,MYAIO                                                         
         MVC   0(L'DRKMAJOR,RF),DRKEY                                           
         XC    L'DRKMAJOR(2,RF),L'DRKMAJOR(RF)                                  
                                                                                
         LA    R1,DHTTTL                                                        
         BAS   RE,COUNTER                                                       
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
** READ FILE RECORDS **                                                         
*                                                                               
         DS    0H                                                               
         GOTO1 DATAMGR,DMCB,READHI,DEMFIL,NDXDA,MYAIO                           
         B     SPLF10B                                                          
SPLF10A  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,READSQ,DEMFIL,NDXDA,MYAIO                           
                                                                                
SPLF10B  DS    0H                                                               
         CLI   8(R1),0                                                          
         BNE   SPLF30                                                           
         LA    R1,FILTTL                                                        
         BAS   RE,COUNTER                                                       
                                                                                
*                                                                               
*** FILTER LOGIC ***                                                            
*                                                                               
         DS    0H                  CHECK FOR RATING SERVICE MKT FILTER          
         OC    FLTRMKT,FLTRMKT          ANY MARKET FILTER?                      
         BZ    SPLF24X                   NO, SKIP MARKET FILTER                 
         L     R3,MYAIO                                                         
         LA    R3,DRFRSTEL-DRKEY(R3)    R3-->FIRST ELEMENT                      
         USING MARELEM,R3                                                       
         CLI   MARCODE,MARCODEQ         IT BETTER BE A MKT ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   FLTRMKT,=C'ML'           LOOK FOR SPECIAL CODE                   
         BE    *+18                      IF IT IS, LOOK FOR MKT IN LST          
         CLC   MARNO,FLTRMKT            MARKET NUMBER MATCH?                    
         BNE   SPLD10A                   NO, READ NEXT DIRECTORY ITEM           
         B     SPLF24X                   YES, PASS MARKET FILTER TEST           
                                                                                
         DS    0H                  MATCH MKT NUMBER TO LIST                     
         LA    RF,FIXMKLST                                                      
SPLF24M  DS    0H                                                               
         OC    0(L'FIXMKLST,RF),0(RF)   END OF TABLE?                           
         BZ    SPLD10A                   YEP, READ NEXT DIRECTORY ITEM          
         CLC   MARNO,0(RF)              MARKET NUMBER MATCH?                    
         BE    SPLF24X                   YEP, PASS MARKET FILTER TEST           
         AHI   RF,L'FIXMKLST                                                    
         B     SPLF24M                                                          
         DROP  R3                                                               
SPLF24X  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  CHECK FOR VALID QH ELEMENT                   
         L     R3,MYAIO                                                         
         LA    R3,DRFRSTEL-DRKEY(R3)   POINT R3 TO 1ST ELEMENT                  
         MVI   ELCODE,QHCODEQ                                                   
                                                                                
*                                                                               
SPLF35   DS    0H                   R3-->AN ELEMENT IN RECORD                   
         BAS   RE,NEXTEL                                                        
         BNE   SPLF10A               EOR REACHED, SKIP THIS RECORD              
                                                                                
         USING QHELEM,R3                                                        
         ZICM  R1,QHWKS,(8)         GET ACTIVE WEEKS                            
         SLL   R1,4                 H.O.4.BITS = ACTIVE WEEKS                   
         LA    RF,1                 ONLY WANT A SINGLE ACTIVE WEEK              
SPLF37B  SR    R0,R0                                                            
         SLDL  R0,1                 SHIFT 1 BIT INTO R0                         
         SR    RF,R0                                                            
         BM    SPLF35               # WEEKS > 1 ==> SKIP THIS QH ELEM           
         OR    R1,R1                                                            
         BNZ   SPLF37B                                                          
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
** PROCESS RECORD **                                                            
*                                                                               
SPL100   DS    0H                                                               
         L     R6,MYAIO                                                         
         USING DRKEY,R6                                                         
         LR    R3,R6               R3 USED TO BUMP THROUGH ELEMENTS             
         LA    R1,DRFRSTEL-DRKEY                                                
         STH   R1,DATADISP                                                      
                                                                                
*                                                                               
         DS    0H                  BUILD KEY OF SORTER RECORD                   
         L     R0,MYAIOSRT                                                      
         LA    R1,IOSRTRL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                CLEAR SORTER'S I/O AREA                     
*                                                                               
         DS    0H                                                               
         L     R4,MYAIOSRT                                                      
         USING INTRECD,R4                                                       
         MVC   INTKSTTN,DRSTAT         STATION                                  
         MVC   INTKKMKT,DRKMKT         SPILL MARKET                             
         MVC   INTKBKY,DRBOOK          BOOK YEAR                                
         MVC   INTKSTYP,DRSTYP         STATION TYPE                             
         MVC   INTKBTYP,DRBTYP         BOOK TYPE                                
         MVC   INTKBKW,DRBOOK+1        BOOK WEEK#                               
         XI    INTKBKW,X'FF'            INVERT IT                               
                                                                                
*                                                                               
         DS    0H                  GET MARKET TYPE ELEMENT                      
         XC    X01ELEM,X01ELEM                                                  
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   SPL109                                                           
         MVC   INTRECLN,=AL2(INTFIXL)  RESET RECORD LENGTH                      
         BAS   RE,BLDSRTRC             BUILD SORT RECORD FOR MKTTYPE EL         
         BAS   RE,PUTSORT              RELEASE RECORD TO SORT                   
SPL109   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  GET QUARTER HOUR ELEMENTS                    
         MVI   ELCODE,QHCODEQ                                                   
*                                                                               
SPL112   DS    0H                  R3-->AN ELEMENT IN THE RECORD                
         BAS   RE,NEXTEL            REACHED END OF RECORD?                      
         BNE   SPLF10A               YEP, READ NEXT RECORD                      
         ST    R3,MYAQUART                                                      
*                                                                               
         DS    0H                  R3-->QUARTER HOUR ELEMENT                    
         USING QHELEM,R3                                                        
         ZICM  R1,QHWKS,(8)         GET ACTIVE WEEKS                            
         SLL   R1,4                 H.O.4.BITS = ACTIVE WEEKS                   
         LA    RF,1                 ONLY WANT A SINGLE ACTIVE WEEK              
SPL114B  SR    R0,R0                                                            
         SLDL  R0,1                 SHIFT 1 BIT INTO R0                         
         SR    RF,R0                                                            
         BM    SPL112               # WEEKS > 1 ==> SKIP THIS QH ELEM           
         OR    R1,R1                                                            
         BNZ   SPL114B                                                          
         DROP  R3                                                               
                                                                                
*                                                                               
         DS    0H                  PROCESS THIS QH ELEMENT                      
         MVC   INTRECLN,=AL2(INTFIXL)  RESET RECORD LENGTH                      
         BAS   RE,BLDSRTRC             BUILD SORT RECORD FOR QH ELEMENT         
         BAS   RE,PUTSORT              RELEASE RECORD TO SORT                   
                                                                                
*                                                                               
         DS    0H                  EXTRACT DEMO VALUES FOR THIS QH ELEM         
         MVI   DEMOEL,DHOMELQ                                                   
         MVI   DEMOPOS,DHOMDSP                                                  
         BAS   RE,GTDEMVAL          GET DHOMES INTO DEMOVAL                     
         MVC   DVDHOMES,DEMOVAL                                                 
                                                                                
         MVI   DEMOEL,UHOMELQ                                                   
         MVI   DEMOPOS,UHOMDSP                                                  
         BAS   RE,GTDEMVAL          GET UHOMES INTO DEMOVAL                     
         MVC   DVUHOMES,DEMOVAL                                                 
*                                                                               
         DS    0H                  BUILD ELEMENT OF DEMO VALUES                 
         XC    MYELEM,MYELEM                                                    
         MVI   MYELEM+0,X'5A'       ELEMENT CODE                                
         MVI   MYELEM+1,10          ELEMENT LENGTH                              
         MVC   MYELEM+2(8),DVDHOMES                                             
*                                                                               
         DS    0H                  PUT DEMO VALUE ELEM INTO SORT RECD           
         MVC   INTRECLN,=AL2(INTFIXL)  RESET RECORD LENGTH                      
                                                                                
         LA    R3,MYELEM                                                        
         BAS   RE,BLDSRTRC                                                      
         L     R3,MYAQUART                                                      
                                                                                
         BAS   RE,PUTSORT                                                       
         DROP  R4                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     SPL112                                                           
                                                                                
*                                                                               
** EOF ON DEMFIL FOR MAJOR KEY **                                               
*                                                                               
SPLF30   DS    0H                                                               
         B     SPLD10A                                                          
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*---------------------------- TAPE OUTPUT ----------------------------*         
*                                                                               
                                                                                
SPLPUT   DS    0H                                                               
         XC    PREVDATA(PREVDATL),PREVDATA                                      
         XC    X01ELEM,X01ELEM                                                  
         XC    X20ELEM,X20ELEM                                                  
         XC    X21ELEM,X21ELEM                                                  
         XC    X51ELEM,X51ELEM                                                  
         XC    X53ELEM,X53ELEM                                                  
*                                                                               
         L     R0,MYAIO                                                         
         LA    R1,IOL                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR IO AREA                                
*                                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   SPLP05X                                                          
         OPEN  (OUT,OUTPUT)                                                     
SPLP05X  EQU   *                                                                
                                                                                
*                                                                               
** FIRST RECORD FROM SORTER **                                                  
*                                                                               
SPLP10   DS    0H                                                               
         BAS   RE,GETSORT          GET FIRST RECORD FROM SORTER                 
         BNE   SPLPUTX              IF NONE, JUST EXIT                          
*^^TEST                                                                         
         BC    0,SPLP10                                                         
*^^EOTEST                                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         L     R2,MYAIOSRT                                                      
         USING INTRECD,R2                                                       
*                                                                               
         LA    R3,INTDATA                                                       
         BAS   RE,BLDELEM          BUILD ELEMENT FROM SORT RECORD               
                                                                                
         BAS   RE,INITDMRC         INITIALIZE DEMO RECORD                       
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
** SUBSEQUENT RECORDS FROM SORTER **                                            
*                                                                               
SPLP20   DS    0H                                                               
         USING INTRECD,R2                                                       
         MVC   PRVSRTKY,INTKEY     CURRENT SORT KEY BECOMES PREVIOUS            
         DROP  R2                                                               
         BAS   RE,GETSORT          GET RECORD FROM SORTER                       
         BNE   SPLP80                                                           
*                                                                               
         L     R2,MYAIOSRT                                                      
         USING INTRECD,R2                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         CLC   PRVSRTKY(SRTKMAJL),INTKEY   SAME MAJOR KEY?                      
         BE    SPLP39                       YES                                 
*                                                                               
         BAS   RE,CONDELEM         CONDENSE DEMO ELEMENTS                       
                                                                                
SPLP22   DS    0H                                                               
         BAS   RE,PUTELEMS         PUT CURRENT ELEMENTS INTO RECORD             
         BE    SPLP22X             DO THEY FIT?                                 
*&&DO                                                                           
         BAS   RE,PUTRECD           NO, PUT CURRENT RECD FIRST,                 
         BAS   RE,INITDMRC           INITIALIZE DEMO RECORD,                    
         B     SPLP22                AND TRY PUTTING CURRNT ELEMS AGAIN         
*&&                                                                             
         DC    H'0'                                                             
SPLP22X  EQU   *                                                                
                                                                                
         BAS   RE,PUTRECD          PUT RECORD                                   
                                                                                
         XC    X01ELEM,X01ELEM     RE-INITIALIZE ALL ELEMENTS                   
         BAS   RE,CLRELEM                                                       
                                                                                
         LA    R3,INTDATA                                                       
         BAS   RE,BLDELEM          BUILD MARKET TYPE ELEMENT                    
                                                                                
         BAS   RE,INITDMRC         INITIALIZE RECORD                            
                                                                                
         B     SPLP20              GO GET NEXT SORT RECORD                      
SPLP39   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         CLC   PRVSRTKY(SRTKWKL),INTKEY   SAME "MINOR" KEY?                     
         BE    SPLP59                      YES                                  
*                                                                               
         BAS   RE,CONDELEM         CONDENSE DEMO ELEMENTS                       
                                                                                
SPLP42   DS    0H                                                               
         BAS   RE,PUTELEMS         PUT CURRENT ELEMENTS INTO RECORD             
         BE    SPLP42X             DO THEY FIT?                                 
*&&DO                                                                           
         BAS   RE,PUTRECD           NO, PUT CURRENT RECD FIRST,                 
         BAS   RE,INITDMRC           INITIALIZE DEMO RECORD,                    
         B     SPLP42                AND TRY PUTTING CURRNT ELEMS AGAIN         
*&&                                                                             
         DC    H'0'                                                             
SPLP42X  EQU   *                                                                
                                                                                
         BAS   RE,PUTRECD          PUT RECORD                                   
                                                                                
         BAS   RE,CLRELEM          RE-INITIALIZE ELEMS W/ QH DATA               
                                                                                
         LA    R3,INTDATA                                                       
         BAS   RE,BLDELEM          BUILD QUARTER HOUR ELEMENT                   
                                                                                
         BAS   RE,INITDMRC         INITIALIZE DEMO RECORD                       
                                                                                
         B     SPLP20              GO GET NEXT SORT RECORD                      
SPLP59   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  CURRENT KEY MATCHES PREVIOUS KEY             
         LA    R3,INTDATA                                                       
         BAS   RE,BLDELEM           JUST BUILD ELEMENT                          
         B     SPLP20                                                           
         EJECT                                                                  
*                                                                               
** PROCESS LAST SORT RECORD **                                                  
*                                                                               
SPLP80   DS    0H                                                               
         BAS   RE,CONDELEM         CONDENSE DEMO ELEMENTS                       
                                                                                
SPLP82   DS    0H                                                               
         BAS   RE,PUTELEMS         PUT CURRENT ELEMENTS INTO RECORD             
         BE    SPLP82X             DO THEY FIT?                                 
         BAS   RE,PUTRECD           NO, PUT CURRENT RECD FIRST,                 
         BAS   RE,INITDMRC           INITIALIZE DEMO RECORD,                    
         B     SPLP82                AND TRY PUTTING CURRNT ELEMS AGAIN         
SPLP82X  EQU   *                                                                
                                                                                
         BAS   RE,PUTRECD          PUT RECORD                                   
         DROP  R2                                                               
*                                                                               
SPLPUTX  DS    0H                                                               
         CLI   QOPT3,C'Y'                                                       
         BNE   SPLPUTX1                                                         
         CLOSE (OUT)                                                            
SPLPUTX1 EQU   *                                                                
*                                                                               
         B     MAINX                                                            
         EJECT                                                                  
*                                                                               
*-------------------------- PRE-EXIT TASKS ---------------------------*         
*                                                                               
MAINX    DS    0H                                                               
                                                                                
         GOTO1 =V(SORTER),DMCB,=C'END',0,0                                      
         GOTO1 REPORT              PRINT BLANK LINE                             
                                                                                
         LA    R2,RPTTBLE                                                       
MAINX10  LA    R0,WRITTL                                                        
         CR    R0,R2                                                            
         BL    EXIT                                                             
         LA    R3,P                                                             
         MVC   0(L'RPTSTMT,R3),L'RPTTTL(R2)                                     
         LA    R3,L'RPTSTMT+1(R3)                                               
         MVI   0(R3),C'='                                                       
         LA    R3,2(R3)                                                         
         L     R1,0(R2)                                                         
         EDIT  (R1),(13,(R3)),COMMAS=YES,ZERO=NOBLANK                           
         GOTO1 REPORT                                                           
         LA    R2,L'RPTTBLE(R2)                                                 
         B     MAINX10                                                          
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
*------------------------- BUILD SORT RECORD -------------------------*         
*                                                                               
* AT ENTRY,                                                                     
*   R4-->INTRECD WITH KEY PARTIALLY BUILT                                       
*   R3-->ELEMENT TO PUT TO SORT                                                 
*   MYAIO = A(DEMO RECORD)                                                      
* !!WARNING!! DO NOT CLOBBER  ELCODE  IN THIS ROUTINE                           
                                                                                
BLDSRTRC NTR1                                                                   
         USING INTRECD,R4                                                       
                                                                                
*                                                                               
** FILL IN THE REST OF KEY **                                                   
*                                                                               
         MVC   INTELCOD,0(R3)                                                   
                                                                                
*                                                                               
         DS    0H                  DETERMINE ELEMENT TYPE                       
         CLI   0(R3),X'01'          MARKET TYPE ELEMENT                         
         BE    BSRMT                                                            
         CLI   0(R3),QHCODEQ        QUARTER-HOUR ELEMENT                        
         BE    BSRQH                                                            
         CLI   0(R3),X'5A'          DEMO VALUE ELEMENT                          
         BE    BSRDV                                                            
         DC    H'0'                                                             
                                                                                
*                                                                               
** APPEND MARKET TYPE ELEMENT **                                                
*                                                                               
BSRMT    DS    0H                                                               
         LR    R6,R3                                                            
         SR    R0,R0                                                            
BSRMT04  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),QHCODEQ       LOOK FOR 1ST (VALID) QH ELEM IN RECD         
         BNE   BSRMT04                                                          
                                                                                
         USING QHELEM,R6                                                        
         ZICM  R1,QHWKS,(8)         GET ACTIVE WEEKS                            
         SLL   R1,4                 H.O.4.BITS = ACTIVE WEEKS                   
         LA    RF,1                 ONLY WANT A SINGLE ACTIVE WEEK              
BSRMT08  SR    R0,R0                                                            
         SLDL  R0,1                 SHIFT 1 BIT INTO R0                         
         SR    RF,R0                                                            
         BM    BSRMT04              # WEEKS > 1 ==> SKIP THIS QH ELEM           
         OR    R1,R1                                                            
         BNZ   BSRMT08                                                          
         DROP  R6                                                               
                                                                                
         USING QHELEM,R6                                                        
         MVC   INTKDAY(3),QHDAY    DAY/QH FROM 1ST VALID QH ELEM OF REC         
         DROP  R6                                                               
*                                                                               
         ZICM  R0,INTRECLN,(3)                                                  
         LR    RE,R0                                                            
         LA    RE,INTRECD(RE)                                                   
         ZIC   R1,1(R3)                                                         
         AR    R0,R1                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),0(R3)                                                   
         STCM  R0,3,INTRECLN                                                    
         B     BSRX                                                             
                                                                                
*                                                                               
** APPEND QUARTER HOUR INFO **                                                  
*                                                                               
*** QUARTER HOUR ELEMENT ***                                                    
*                                                                               
BSRQH    DS    0H                                                               
         USING QHELEM,R3                                                        
*                                                                               
         DS    0H                                                               
         MVC   INTKDAY,QHDAY        SET THESE                                   
         MVC   INTKQHS,QHSQH         KEY FIELDS                                 
*&&DO                                                                           
         MVC   INTKWKS,QHWKS                                                    
*&&                                                                             
*                                                                               
         DS    0H                   QUARTER HOUR ELEMENT                        
         LA    R5,QHELEM                                                        
         LR    RF,R5                RF-->SOURCE FOR PROGRAM NAME                
         CLI   1(RF),6                                                          
         BH    *+8                                                              
         LA    RF,X20ELEM           PROGRAM NAME IN PREV QH ELEM                
                                                                                
         ZICM  R0,INTRECLN,(3)                                                  
         LR    RE,R0                                                            
         LA    RE,INTRECD(RE)       RE-->DESTINATION IN SORT RECORD             
                                                                                
         MVC   0(6,RE),QHELEM       DAYS/QHS/WEEK INFO                          
         ZIC   R1,1(RF)                                                         
         BCTR  R1,0                                                             
         SH    R1,=H'6'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EXMVC R1,6(RE),6(RF)       PROGRAM NAME                                
                                                                                
         LA    R1,6+1(R1)                                                       
         AR    R0,R1                                                            
         STCM  R0,3,INTRECLN        UPDATE RECORD LENGTH                        
                                                                                
         CLI   QHELN,6                                                          
         BNH   BSRQH30X                                                         
         XC    X20ELEM,X20ELEM                                                  
         ZIC   R1,QHELN                                                         
         BCTR  R1,0                                                             
         EXMVC R1,X20ELEM,QHELEM    (SO WE'LL HAVE A PREV QH ELEM)              
BSRQH30X EQU   *                                                                
         DROP  R3                                                               
                                                                                
*                                                                               
*** QUARTER HOUR PROGRAM INFO ELEMENT ***                                       
*                                                                               
         DS    0H                  QH PROGRAM INFO ELEMENT                      
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                BUMP PAST QH ELEMENT                        
         CLI   0(R5),X'21'          IS THIS QH PROGRAM INFO ELEMENT?            
         BNE   BSRQH49               NO, FORGET IT                              
                                                                                
         LR    RF,R5                RF-->SOURCE FOR PROGRAM INFO                
         CLI   1(RF),4                                                          
         BH    BSRQH44X                                                         
         ZICM  RF,2(R5),(3)                                                     
         SLL   RF,17                REMOVE HIGH ORDER BIT                       
         SRL   RF,17                                                            
         A     RF,MYAIO             USE A PREVIOUS PROGRAM INFO ELEM            
BSRQH44X EQU   *                                                                
                                                                                
         ZICM  R0,INTRECLN,(3)                                                  
         LR    RE,R0                R0 = L(SORT RECORD), SO FAR                 
         LA    RE,INTRECD(RE)       RE-->END OF RECORD                          
                                                                                
         ZIC   R1,1(RF)             R1 = L(QH PROGRAM INFO ELEM)                
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),0(RF)                                                   
                                                                                
         LA    R1,1(R1)                                                         
         AR    R0,R1                                                            
         STCM  R0,3,INTRECLN       UPDATE SORT RECORD LENGTH                    
BSRQH49  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
*^^TEST                                                                         
         CLI   0(RE),X'21'         IT STILL BETTER BE A X'21' ELEMENT           
         BE    *+6                                                              
         DC    H'0'                                                             
*^^EOTEST                                                                       
         B     BSRX                                                             
                                                                                
*                                                                               
** DEMO VALUE ELEMENT **                                                        
*                                                                               
BSRDV    DS    0H                                                               
         ZICM  R0,INTRECLN,(3)                                                  
         LR    RE,R0                                                            
         LA    RE,INTRECD(RE)                                                   
         ZIC   R1,1(R3)                                                         
         AR    R0,R1                                                            
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),0(R3)                                                   
         STCM  R0,3,INTRECLN                                                    
         B     BSRX                                                             
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
BSRX     DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*--------------------------- GET DEMO VALUE --------------------------*         
*                                                                               
* AT ENTRY,                                                                     
*   MYAIO   = A(DEMO RECORD)                                                    
*   DEMOEL  = ELEMENT CODE OF ELEMENT TO PULL DEMO VALUE FROM                   
*   DEMOPOS = POSITION OF DEMO VALUE WITHIN ELEMENT                             
*   R3     -->QUARTER HOUR ELEMENT                                              
* AT EXIT,                                                                      
*   DEMOVAL = DEMO VALUE RETURNED TO CALLER                                     
* !!WARNING!! DO NOT CLOBBER  ELCODE  IN THIS ROUTINE                           
                                                                                
GTDEMVAL NTR1                                                                   
         XC    DEMOVAL,DEMOVAL                                                  
                                                                                
*                                                                               
         DS    0H                                                               
         LR    RF,R3                                                            
         SR    R0,R0                                                            
                                                                                
GDV012   DS    0H                  FIND DEMO ELEMENT                            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),0              EOR?                                        
         BE    GDVX                  YES, EXIT                                  
         CLI   0(RF),QHCODEQ        REACHED NEXT QH ELEM?                       
         BE    GDVX                  YES, EXIT                                  
         CLC   0(1,RF),DEMOEL       IS THIS THE ELEMENT WE'RE SEEKING?          
         BNE   GDV012               NO, TRY NEXT ELEMENT                        
*                                                                               
         DS    0H                  GET R4 TO POINT TO DEMO ELEMENT              
         LR    R4,RF                                                            
         CLI   1(RF),4              DUPLICATED ELEMENT?                         
         BNE   GDV019                NO                                         
         TM    2(RF),X'80'          DUPLICATED ELEMENT?                         
         BZ    GDV019                NO                                         
         SR    R4,R4                 YES, ADJUST POINTER                        
         ICM   R4,12,2(RF)            INSERT TO H.O.2.B OF R4                   
         SLL   R4,1                   REMOVE H.O.B.                             
         SRL   R4,17                                                            
         A     R4,MYAIO                                                         
GDV019   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  EXTRACT DEMO VALUE FROM ELEMENT              
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),DEMOPOS                                                
*                                                                               
         ZICM  RE,2(R4),(8)                                                     
         SLL   RE,4                                                             
         SRL   RE,28                RE = # BYTES PER DEMO IN ELEMENT            
         LR    R5,RE                                                            
         MH    R5,HALF                                                          
         LA    R5,3(R5,R4)          R5-->DEMO VALUE                             
*                                                                               
         A     RE,AMASKTAB          RE-->MASK FOR  ICM  INSTRUCTION             
         ZIC   RF,0(RE)                                                         
         SR    R0,R0                                                            
         EX    RF,*+8                                                           
         B     *+8                                                              
         ICM   R0,0,0(R5)           R0 = DEMO VALUE                             
         TM    2(R4),X'40'         ADJ FOR DECIMAL                              
         BO    *+8                                                              
         MH    R0,=H'10'                                                        
         ST    R0,DEMOVAL                                                       
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
GDVX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*--------------------------- BUILD ELEMENT ---------------------------*         
*                                                                               
* AT ENTRY,                                                                     
*   R3-->ELEMENT TO PULL DATA FROM                                              
* !!WARNING!! DO NOT CLOBBER  ELCODE  IN THIS ROUTINE                           
                                                                                
BLDELEM  NTR1                                                                   
         DS    0H                  DETERMINE ELEMENT TYPE                       
         CLI   0(R3),X'01'          MARKET TYPE ELEMENT                         
         BE    BELMT                                                            
         CLI   0(R3),QHCODEQ        QUARTER-HOUR ELEMENT                        
         BE    BELQH                                                            
         CLI   0(R3),X'5A'          DEMO VALUE ELEMENT                          
         BE    BELDV                                                            
         DC    H'0'                                                             
                                                                                
*                                                                               
** BUILD MARKET TYPE ELEMENT **                                                 
*                                                                               
BELMT    DS    0H                                                               
         OC    X01ELEM,X01ELEM                                                  
         BNZ   *+20                                                             
         MVC   X01ELEM,0(R3)                                                    
         MVC   X01ELEM+(MARDATE-MARELEM)(L'MARDATE),TODAYP                      
         B     BELX                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         USING MARELEM,R3                                                       
X01E     USING MARELEM,X01ELEM                                                  
         CLC   X01E.MARNO(4),MARNO   MKT/MKT-TYPE/STTN-TYPE S/B MATCH           
*        BE    *+6                                                              
*        DC    H'0'                                                             
         DROP  R3,X01E                                                          
         B     BELX                                                             
                                                                                
*                                                                               
** BUILD QUARTER HOUR ELEMENTS **                                               
*                                                                               
BELQH    DS    0H                                                               
         OC    X20ELEM,X20ELEM     IS THERE ONE IN PROCESS?                     
         BZ    BELQH19              NOPE                                        
         CLC   X20ELEM+2(4),2(R3)   YEP, GUARD AGAINST MISMATCH                 
         BE    *+6                                                              
         DC    H'0'                                                             
         B     BELX                                                             
BELQH19  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  BUILD QUARTER HOUR ELEMENT                   
         LA    R4,X20ELEM                                                       
         USING QHELEM,R4                                                        
         MVC   QHELEM(6),0(R3)      OVERHEAD STUFF                              
         MVC   QHPNAME(14),SPACES   PROGRAM NAME                                
*                                                                               
         DS    0H                   DAY                                         
         L     RF,ADAYTAB                                                       
BELQH32  CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   QHDAY,0(RF)                                                      
         BE    *+12                                                             
         LA    RF,L'DAYTAB(RF)                                                  
         B     BELQH32                                                          
         MVC   QHPNAME(3),1(RF)                                                 
*                                                                               
         MVI   QHPNAME+3,C'/'                                                   
*                                                                               
         DS    0H                   TIME                                        
         SR    R0,R0                                                            
         ZIC   R1,QHSQH                                                         
         D     R0,=F'4'                                                         
         LA    R1,6(R1)              BASE = 6AM                                 
         CH    R1,=H'24'             TEST AFTER MIDNIGHT                        
         BNH   *+8                                                              
         SH    R1,=H'24'              YES - GO BACK ONE DAY                     
         MH    R1,=H'100'                                                       
         MH    R0,=H'15'                                                        
         AR    R1,R0                 R1 CONTAINS MILITARY TIME                  
         SLL   R1,16                 SET AS START AND "END" TIMES               
         STCM  R1,15,MTIMES          STORE START/END TIMES IN  FULL             
                                                                                
         MVC   WORK,SPACES                                                      
         GOTO1 UNTIME,DMCB,MTIMES,WORK                                          
         MVC   QHPNAME+4(5),WORK                                                
*                                                                               
         DS    0H                  ELEMENT LENGTH                               
         LA    RF,QHPNAME+QHPNMAX-1                                             
         CLI   0(RF),X'40'                                                      
         BH    *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         LA    RF,1(RF)                                                         
         LA    R0,QHELEM                                                        
         SR    RF,R0                                                            
         STC   RF,QHELN                                                         
*&&DO                                                                           
         DS    0H                   WEEK                                        
         TM    QHWKS,X'08'                                                      
         BZ    *+8                                                              
         MVI   QHPNAME+10,C'1'                                                  
         TM    QHWKS,X'04'                                                      
         BZ    *+8                                                              
         MVI   QHPNAME+11,C'2'                                                  
         TM    QHWKS,X'02'                                                      
         BZ    *+8                                                              
         MVI   QHPNAME+12,C'3'                                                  
         TM    QHWKS,X'01'                                                      
         BZ    *+8                                                              
         MVI   QHPNAME+13,C'4'                                                  
*                                                                               
         MVI   QHELN,((QHPNAME+14)-QHELEM)                                      
*&&                                                                             
         DROP  R4                                                               
                                                                                
*                                                                               
         DS    0H                  BUILD PROGRAM INFO ELEMENT                   
         LA    R4,X21ELEM           R4-->PLACE TO BUILD PGM INFO ELEM           
         USING QIELEM,R4                                                        
         ZIC   R5,1(R3)                                                         
         AR    R5,R3                R5-->SOURCE FOR PROGRAM INFO ELEM           
         ZIC   R1,1(R5)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,QIELEM,0(R5)                                                  
*^^TEST                                                                         
         CLI   QIELEM,X'21'        IT STILL BETTER BE A X'21' ELEMENT           
         BE    *+6                                                              
         DC    H'0'                                                             
*^^EOTEST                                                                       
                                                                                
         XC    QIPNUM,QIPNUM                                                    
         XC    QIPTYPE,QIPTYPE                                                  
         MVC   QIPNAM6+0(2),X20ELEM+(QHPNAME-QHELEM)                            
         CLC   QIPNAM6+0(2),=C'M-'   CHANGE M-F                                 
         BNE   *+8                                                              
         MVI   QIPNAM6+1,C'F'         TO MF                                     
         ZICM  R0,MSTIME,(3)                                                    
         CH    R0,=H'2400'                                                      
         BNH   *+8                                                              
         SH    R0,=H'2400'                                                      
         CVD   R0,DUB                                                           
         UNPK  QIPNAM6+2(4),DUB                                                 
         OI    QIPNAM6+5,X'F0'                                                  
         DROP  R4                                                               
                                                                                
*                                                                               
         B     BELX                                                             
                                                                                
*                                                                               
** BUILD DEMO VALUE ELEMENTS **                                                 
*                                                                               
BELDV    DS    0H                                                               
         CLI   X51ELEM,X'51'       IS THERE ONE IN PROCESS?                     
         BE    BELDV19              YEP                                         
         MVI   X51ELEM+0,X'51'       INIT DHOMES ELEMENT                        
         MVI   X51ELEM+1,3+4                                                    
         MVI   X53ELEM+0,X'53'       INIT UHOMES ELEMENT                        
         MVI   X53ELEM+1,3+4                                                    
BELDV19  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  INSERT DEMO VALUES INTO ELEMENT              
         L     R6,MYAIOSRT                                                      
         USING INTRECD,R6                                                       
         ZIC   R1,INTKBKW           R1 = WEEK NUMBER                            
         DROP  R6                                                               
*                                                                               
         SLL   R1,2                 MULTIPLY BY FOUR                            
         LA    R1,3(R1)             ACCOUNT FOR ELEMENT OVERHEAD                
         STH   R1,HALF              HALF = DISPL INTO ELEM TO STORE VAL         
                                                                                
*                                                                               
         DS    0H                   DHOMES                                      
         ICM   RE,15,2(R3)           RE = DHOMES VALUE                          
         BZ    BELDV39                                                          
                                                                                
         LA    R1,X51ELEM                                                       
         CLM   RE,15,3(R1)           COMPARE AGAINST MAX VALUE SO FAR           
         BNH   *+8                                                              
         STCM  RE,15,3(R1)            UPDATE MAX VALUE                          
         LR    RF,R1                                                            
         AH    RF,HALF                                                          
         STCM  RE,15,0(RF)           STORE VALUE IN ITS POSITION                
         LA    RF,4(RF)                                                         
         SR    RF,R1                                                            
         CLM   RF,1,1(R1)            COMPARE AGAINST L(ELEMENT)                 
         BNH   *+8                                                              
         STC   RF,1(R1)               UPDATE NEW LENGTH OF ELEMENT              
BELDV39  EQU   *                                                                
*                                                                               
         DS    0H                   UHOMES                                      
         ICM   RE,15,6(R3)           RE = UHOMES VALUE                          
         BZ    BELDV49                                                          
                                                                                
         LA    R1,X53ELEM                                                       
         CLM   RE,15,3(R1)           COMPARE AGAINST MAX VALUE SO FAR           
         BNH   *+8                                                              
         STCM  RE,15,3(R1)            UPDATE MAX VALUE                          
         LR    RF,R1                                                            
         AH    RF,HALF                                                          
         STCM  RE,15,0(RF)           STORE VALUE IN ITS POSITION                
         LA    RF,4(RF)                                                         
         SR    RF,R1                                                            
         CLM   RF,1,1(R1)            COMPARE AGAINST L(ELEMENT)                 
         BNH   *+8                                                              
         STC   RF,1(R1)               UPDATE NEW LENGTH OF ELEMENT              
BELDV49  EQU   *                                                                
*                                                                               
         B     BELX                                                             
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
BELX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*-------------------------- CONDENSE ELEMENT -------------------------*         
*                                                                               
* CONDENSES THE DHOMES (X'51') AND UHOMES (X'53') ELEMENTS                      
                                                                                
CONDELEM NTR1                                                                   
*                                                                               
** DHOMES (X'51') ELEMENT **                                                    
*                                                                               
         CLI   X51ELEM,X'51'       ELEMENT INITIALIZED YET?                     
         BNE   CNDEL29              NOPE                                        
                                                                                
*                                                                               
         DS    0H                                                               
         SR    R1,R1                R1 COUNTS # OF BYTES IN MAX VALUE           
         LA    R0,1                                                             
         ICM   RF,15,X51ELEM+3      RF = MAX DHOMES VALUE                       
         BNZ   *+14                                                             
         XC    X51ELEM,X51ELEM       IF MAX DHOMES IS ZERO,                     
         B     CNDEL29                NO ELEMENT NEEDED FOR DHOMES              
*                                                                               
CNDEL12  DS    0H                                                               
         AR    R1,R0               UPDATE # OF BYTES                            
         SRL   RF,8                DIVIDE MAX VALUE BY 256                      
         OR    RF,RF                UNTIL VALUE BECOMES ZERO                    
         BNZ   CNDEL12                                                          
*                                                                               
         STC   R1,X51ELEM+2         STORE # OF BYTES                            
         LA    RF,4                                                             
         SR    RF,R1                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         ZIC   R0,X51ELEM+1                                                     
         SH    R0,=Y(3+4)                                                       
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,X51ELEM+3         R2-->DEST FOR CONDENSED VALUES              
         LA    R3,4(RF,R2)          R3-->SOURCE OF DEMO VALUES                  
         BCTR  R1,0                                                             
                                                                                
CNDEL18  DS    0H                                                               
         EXMVC R1,0(R2),0(R3)                                                   
         LA    R2,1(R2,R1)          BUMP TO NEXT DEST SLOT                      
         LA    R3,4(R3)             BUMP TO NEXT SOURCE                         
         SH    R0,=H'4'                                                         
         BP    CNDEL18                                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,X51ELEM                                                       
         SR    R2,R0                                                            
         STC   R2,X51ELEM+1         UPDATE NEW ELEMENT LENGTH                   
CNDEL29  EQU   *                                                                
                                                                                
*                                                                               
** UHOMES (X'53') ELEMENT **                                                    
*                                                                               
         CLI   X53ELEM,X'53'       ELEMENT INITIALIZED YET?                     
         BNE   CNDEL49              NOPE                                        
                                                                                
*                                                                               
         DS    0H                                                               
         SR    R1,R1                R1 COUNTS # OF BYTES IN MAX VALUE           
         LA    R0,1                                                             
         ICM   RF,15,X53ELEM+3      RF = MAX UHOMES VALUE                       
         BNZ   *+14                                                             
         XC    X53ELEM,X53ELEM       IF MAX UHOMES IS ZERO,                     
         B     CNDEL49                NO ELEMENT NEEDED FOR UHOMES              
*                                                                               
CNDEL32  DS    0H                                                               
         AR    R1,R0                UPDATE # OF BYTES                           
         SRL   RF,8                 DIVIDE MAX VALUE BY 256                     
         OR    RF,RF                 UNTIL VALUE BECOMES ZERO                   
         BNZ   CNDEL32                                                          
*                                                                               
         STC   R1,X53ELEM+2         STORE # OF BYTES                            
         LA    RF,4                                                             
         SR    RF,R1                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         ZIC   R0,X53ELEM+1                                                     
         SH    R0,=Y(3+4)                                                       
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,X53ELEM+3         R2-->DEST FOR CONDENSED VALUES              
         LA    R3,4(RF,R2)          R3-->SOURCE OF DEMO VALUES                  
         BCTR  R1,0                                                             
                                                                                
CNDEL38  DS    0H                                                               
         EXMVC R1,0(R2),0(R3)                                                   
         LA    R2,1(R2,R1)          BUMP TO NEXT DEST SLOT                      
         LA    R3,4(R3)             BUMP TO NEXT SOURCE                         
         SH    R0,=H'4'                                                         
         BP    CNDEL38                                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,X53ELEM                                                       
         SR    R2,R0                                                            
         STC   R2,X53ELEM+1         UPDATE NEW ELEMENT LENGTH                   
CNDEL49  EQU   *                                                                
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
CNDELX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*---------------------------- PUT ELEMENT ----------------------------*         
*                                                                               
* PUTS QH INFO AND DEMO VALUES INTO DEMO RECORD                                 
* AT ENTRY,                                                                     
*   MYAIO = A(DEMO RECORD TO PUT ELEMENTS IN)                                   
* AT EXIT,                                                                      
*   CC SET TO EQ  IF ELEMENTS FIT INTO RECORD                                   
*   CC SET TO NEQ IF ELEMENTS WERE NOT PUT INTO RECORD                          
                                                                                
PUTELEMS NTR1                                                                   
         L     R2,MYAIO                                                         
         LA    R2,4(R2)                                                         
         USING DRKEY,R2                                                         
                                                                                
*                                                                               
         DS    0H                                                               
         ZICM  R3,DRRLEN,(3)       MUST HAVE SOMETHING IN RECD BY NOW           
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         BCTR  R3,0                                                             
         AR    R3,R2               R3-->NEXT AVAILABLE SPACE IN RECORD          
                                                                                
*                                                                               
         DS    0H                  MOVE QUARTER HOUR ELEMENT                    
         CLI   X20ELEM,X'20'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,X20ELEM                                                       
         BAS   RE,PELMOVE                                                       
                                                                                
*                                                                               
         DS    0H                  MOVE PROGRAM INFO ELEMENT                    
         CLI   X21ELEM,X'21'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,X21ELEM                                                       
*&&DO                                                                           
         BAS   RE,PELMOVE                                                       
*&&                                                                             
PEL39    EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  MOVE  DHOMES  ELEMENT                        
         CLI   X51ELEM,X'51'        IF NONE,                                    
         BNE   PEL49                 SKIP THIS ELEMENT                          
*                                                                               
         SR    R0,R0                LOOK FOR ANY DUPLICATES IN RECD             
         LA    RF,DRFRSTEL                                                      
PEL42    CR    RF,R3                 REACHED END OF RECORD?                     
         BNL   PEL44X                 YEP, NO DUPLICATES                        
         CLC   0(2,RF),X51ELEM       MATCH ON ELCODE & ELLENGTH FIRST           
         BNE   PEL44                  NO MATCH, BUMP TO NEXT ELEM               
         ZIC   R1,X51ELEM+1                                                     
         BCTR  R1,0                                                             
         EXCLC R1,0(RF),X51ELEM                                                 
         BNE   PEL44                  NO MATCH, BUMP TO NEXT ELEM               
         SR    RF,R2                  FOUND A MATCHING ELEMENT                  
         STCM  RF,3,X51ELEM+2          SET DISPLACEMENT                         
         OI    X51ELEM+2,X'80'                                                  
         MVI   X51ELEM+1,4             L(DUPLICATE ELEM) = 4                    
         B     PEL44X                                                           
                                                                                
PEL44    DS    0H                    BUMP TO NEXT ELEMENT                       
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     PEL42                                                            
PEL44X   EQU   *                                                                
*                                                                               
         LA    RF,X51ELEM                                                       
         BAS   RE,PELMOVE                                                       
PEL49    EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  MOVE  UHOMES  ELEMENT                        
         CLI   X53ELEM,X'53'        IF NONE,                                    
         BNE   PEL59                 SKIP THIS ELEMENT                          
*                                                                               
         SR    R0,R0                LOOK FOR ANY DUPLICATES IN RECD             
         LA    RF,DRFRSTEL                                                      
PEL52    CR    RF,R3                 REACHED END OF RECORD?                     
         BNL   PEL54X                 YEP, NO DUPLICATES                        
         CLC   0(2,RF),X51ELEM       MATCH ON ELCODE & ELLENGTH FIRST           
         BNE   PEL54                  NO MATCH, BUMP TO NEXT ELEM               
         ZIC   R1,X53ELEM+1                                                     
         BCTR  R1,0                                                             
         EXCLC R1,0(RF),X53ELEM                                                 
         BNE   PEL54                  NO MATCH, BUMP TO NEXT ELEM               
         SR    RF,R2                  FOUND A MATCHING ELEMENT                  
         STCM  RF,3,X53ELEM+2          SET DISPLACEMENT                         
         OI    X53ELEM+2,X'80'                                                  
         MVI   X53ELEM+1,4             L(DUPLICATE ELEM) = 4                    
         B     PEL54X                                                           
                                                                                
PEL54    DS    0H                    BUMP TO NEXT ELEMENT                       
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     PEL52                                                            
PEL54X   EQU   *                                                                
*                                                                               
         LA    RF,X53ELEM                                                       
         BAS   RE,PELMOVE                                                       
PEL59    EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  MOVE BOOK (X'5E') ELEMENT                    
         LA    RF,BOOKELEM                                                      
         BAS   RE,PELMOVE                                                       
                                                                                
*                                                                               
** DONE PUTTING ELEMENTS INTO DEMO RECORD **                                    
*                                                                               
PEL60    DS    0H                                                               
         MVI   0(R3),0             NEW EOR MARKER                               
         LA    R3,1(R3)                                                         
         SR    R3,R2               R3 = NEW LENGTH OF RECORD                    
         CH    R3,MAXRLEN          IF GREATER THAN MAX ALLOWED,                 
         BNH   PEL65                                                            
         ZICM  R1,DRRLEN,(3)                                                    
         BCTR  R1,0                                                             
         AR    R1,R2                                                            
         MVI   0(R1),0              RESTORE ORIGINAL END OF RECORD              
         B     PELXN                                                            
*                                                                               
PEL65    DS    0H                  ELEMENTS FIT INTO RECORD                     
         STCM  R3,3,DRRLEN                      UPDATE RECORD LENGTH            
         MVC   DRKMINOR,X20ELEM+(QHDAY-QHELEM)   SET MINOR KEY                  
*                                                                               
         LA    R3,4(R3)                                                         
         L     R1,MYAIO                                                         
         STCM  R3,3,0(R1)          UPDATE IBM RECORD LENGTH                     
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
PELXY    DS    0H                                                               
         B     YES                                                              
*                                                                               
PELXN    DS    0H                                                               
         B     NO                                                               
         DROP  R2                                                               
                                                                                
                                                                                
                                                                                
* LITTLE HELPER ROUTINE TO MOVE ELEMENT INTO RECORD                             
* AT ENTRY,                                                                     
*   RF-->ELEMENT TO PUT INTO RECORD                                             
*   R3-->DESTINATION                                                            
* AT EXIT,                                                                      
*   R3-->NEXT BYTE WITHIN RECORD AFTER THE JUST-MOVED ELEMENT                   
* !!WARNING!! R1 WILL BE CLOBBERED HERE, BUT DO NOT CLOBBER RE                  
                                                                                
PELMOVE  DS    0H                                                               
         ZICM  R1,1(RF),(1)                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),0(RF)                                                   
         LA    R3,1(R3,R1)                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*----------------------------- PUT RECORD ----------------------------*         
*                                                                               
* AT ENTRY,                                                                     
*   MYAIO = A(DEMO RECORD)                                                      
                                                                                
PUTRECD  NTR1                                                                   
         L     R2,MYAIO                                                         
         OC    0(2,R2),0(R2)                                                    
         BZ    PUTRX                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   QOPT2,C'Y'          PRINT DEMO RECORD?                           
         BNE   PUTR019              NO                                          
*                                                                               
         LA    R3,4(R2)                                                         
         USING DRKEY,R3                                                         
         ZICM  R1,DRRLEN,(3)                                                    
         CLC   DRRLEN,=Y(L'P)                                                   
         BNH   *+8                                                              
         LA    R1,L'P                                                           
                                                                                
         BCTR  R1,0                                                             
         EXMVC R1,P,DRKEY                                                       
         GOTO1 REPORT                                                           
         DROP  R3                                                               
PUTR019  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  PUT DEMO RECORD TO TAPE                      
         CLI   QOPT3,C'Y'                                                       
         BNE   PUTR029                                                          
         PUT   OUT,(R2)                                                         
PUTR029  EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  UPDATE NUMBER OF RECORDS WRITTEN             
         LA    R1,WRITTL                                                        
         BAS   RE,COUNTER                                                       
                                                                                
*                                                                               
** EXIT **                                                                      
*                                                                               
PUTRX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*--------------------------- CLEAR ELEMENTS --------------------------*         
*                                                                               
* CLEARS OUT ALL THOSE ELEMENTS THAT CONTAIN DATA WHICH PERTAINS TO             
*  A SPECIFIC QUARTER HOUR                                                      
                                                                                
CLRELEM  NTR1                                                                   
         XC    X20ELEM,X20ELEM                                                  
         XC    X21ELEM,X21ELEM                                                  
         XC    X51ELEM,X51ELEM                                                  
         XC    X53ELEM,X53ELEM                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*----------------------- INITIALIZE DEMO RECORDS ---------------------*         
*                                                                               
* AT ENTRY,                                                                     
*   R2-->SORT RECORD                                                            
*   X01ELEM CONTAINS MARKET TYPE ELEMENT                                        
                                                                                
INITDMRC NTR1                                                                   
         CLI   X01ELEM,X'01'       MAKE SURE WE HAVE MARKET TYPE ELEM           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         L     R0,MYAIO                                                         
         LA    R1,IOL                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR I/O AREA                               
                                                                                
*                                                                               
         DS    0H                                                               
         L     R6,MYAIO                                                         
         LA    R6,4(R6)                                                         
         USING DRKEY,R6                                                         
*                                                                               
         DS    0H                  SET KEY                                      
         MVC   DRKEY(3),=C'RWN'                                                 
         USING INTRECD,R2                                                       
         MVC   DRSTAT,INTKSTTN                                                  
         MVC   DRBOOK+0(1),INTKBKY                                              
         XI    DRBOOK+1,X'FF'                                                   
         MVC   DRKMKT,INTKKMKT                                                  
         MVC   DRSTYP,INTKSTYP                                                  
         MVC   DRBTYP,INTKBTYP                                                  
         DROP  R2                                                               
*                                                                               
         DS    0H                  MARKET TYPE ELEMENT                          
         ZIC   R1,X01ELEM+1                                                     
         BCTR  R1,0                                                             
         EXMVC R1,DRFRSTEL,X01ELEM                                              
         LA    R1,DRFRSTEL+1(R1)                                                
*                                                                               
         DS    0H                  EOR MARKER                                   
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
*                                                                               
         DS    0H                  RECORD LENGTH                                
         SR    R1,R6                                                            
         STCM  R1,3,DRRLEN                                                      
         DROP  R6                                                               
                                                                                
*                                                                               
         DS    0H                  IBM RECORD LENGTH                            
         LA    R1,4(R1)                                                         
         L     RF,MYAIO                                                         
         STCM  R1,3,0(RF)                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== GETEL ===============================*         
         GETEL R3,DATADISP,ELCODE                                               
         SPACE 3                                                                
*======================= RELEASE RECORD TO SORT ======================*         
PUTSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',MYAIOSRT                                 
         LA    R1,SINTTL                                                        
         BAS   RE,COUNTER                                                       
         B     EXIT                                                             
         SPACE 3                                                                
*======================== GET RECORD FROM SORT =======================*         
GETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RE,15,DMCB+4                                                     
         BZ    NO                                                               
         ZICM  RF,0(RE),(3)                                                     
         L     R0,MYAIOSRT                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   QOPT1,C'Y'          PRINT SORT RECORD?                           
         BNE   GSRT19               NO                                          
                                                                                
         L     R2,MYAIOSRT                                                      
         USING INTRECD,R2                                                       
         ZICM  R1,INTRECLN,(3)                                                  
         CLC   INTRECLN,=Y(L'P)                                                 
         BNH   *+8                                                              
         LA    R1,L'P                                                           
                                                                                
         BCTR  R1,0                                                             
         EXMVC R1,P,INTRECD                                                     
                                                                                
         GOTO1 REPORT                                                           
         DROP  R2                                                               
GSRT19   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         B     YES                                                              
         SPACE 3                                                                
*============================== COUNTER ==============================*         
COUNTER  DS    0H                                                               
         L     RF,0(R1)                                                         
         LA    RF,1(RF)                                                         
         ST    RF,0(R1)                                                         
         BR    RE                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
         SPACE 2                                                                
OUT      DCB   DDNAME=OUT,DSORG=PS,RECFM=VB,MACRF=PM,                  +        
               LRECL=IOL,BLKSIZE=8200                                           
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,16,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2004'                                  
TESTBOOK DC    AL2(XMAY96)                                                      
BOOKELEM DC    XL7'5E07E3E6D55701'                                              
         SPACE 2                                                                
MAXRLEN  DC    H'1000'             MAX RECORD LENGTH                            
         SPACE 2                                                                
READHI   DC    CL7'DMRDHI'                                                      
READSQ   DC    CL7'DMRSEQ'                                                      
DEMDIR   DC    CL7'DEMDIR'                                                      
DEMFIL   DC    CL7'DEMFIL'                                                      
CMKT     DC    4C'0'                                                            
SPLRRA   DC    F'0'                                                             
SPLRRB   DC    F'0'                                                             
SPLRRC   DC    F'0'                                                             
         SPACE 2                                                                
RPTTBLE  DS    0CL(L'RPTTTL+L'RPTSTMT)                                          
RPTTTL   DS    0F                                                               
RPTSTMT  DS    0CL40                                                            
DIRTTL   DC    F'0',CL40'TOTAL DIRECTORY READS'                                 
DHTTTL   DC    F'0',CL40'TOTAL DIRECTORY HITS'                                  
FILTTL   DC    F'0',CL40'TOTAL FILE READS'                                      
SINTTL   DC    F'0',CL40'TOTAL RECORDS INTO SORT'                               
WRITTL   DC    F'0',CL40'TOTAL RECORDS WRITTEN'                                 
FILSBTTL DC    F'0',CL40'TOTAL RECORDS FOR THIS MAJOR KEY'                      
         EJECT                                                                  
*                                  TABLE OF DISPLACEMENTS                       
DISPTAB  DS    0XL(1+2+2)                                                       
         DC    C'W',AL2(IO-WORKD),AL2(MYAIO-WORKD)                              
         DC    C'W',AL2(IOSRTR-WORKD),AL2(MYAIOSRT-WORKD)                       
         DC    C'C',AL2(MASKTAB-SPLR42),AL2(AMASKTAB-WORKD)                     
         DC    C'C',AL2(DAYTAB-SPLR42),AL2(ADAYTAB-WORKD)                       
DISPTABQ EQU   (*-DISPTAB)/(L'DISPTAB)                                          
                                                                                
                                                                                
MASKTAB  DC    AL1(00,01,03,07,15)                                              
                                                                                
                                                                                
DAYTAB   DS    0XL(1+3)                                                         
         DC     X'10',C'MON'        MONDAY                                      
         DC     X'20',C'TUE'        TUESDAY                                     
         DC     X'30',C'WED'        WEDNESDAY                                   
         DC     X'40',C'THU'        THURSDAY                                    
         DC     X'50',C'FRI'        FRIDAY                                      
         DC     X'60',C'SAT'        SATDAY                                      
         DC     X'70',C'SUN'        SUNDAY                                      
         DC     X'95',C'M-F'        MON-FRI                                     
         DC    X'00'                                                            
         EJECT                                                                  
*========= LIST OF MARKETS THAT NEEDS TO BE FIXED (DEC08/99) =========*         
                                                                                
FIXMKLST DS    0XL2                                                             
         DC     AL2(0101)                                                       
         DC     AL2(0104)                                                       
         DC     AL2(0105)                                                       
         DC     AL2(0106)                                                       
         DC     AL2(0108)                                                       
         DC     AL2(0111)                                                       
         DC     AL2(0112)                                                       
         DC     AL2(0115)                                                       
         DC     AL2(0121)                                                       
         DC     AL2(0124)                                                       
         DC     AL2(0127)                                                       
         DC     AL2(0129)                                                       
         DC     AL2(0133)                                                       
         DC     AL2(0135)                                                       
         DC     AL2(0139)                                                       
         DC     AL2(0148)                                                       
         DC     AL2(0161)                                                       
         DC     AL2(0202)                                                       
         DC     AL2(0209)                                                       
         DC     AL2(0213)                                                       
         DC     AL2(0216)                                                       
         DC     AL2(0217)                                                       
         DC     AL2(0218)                                                       
         DC     AL2(0222)                                                       
         DC     AL2(0223)                                                       
         DC     AL2(0230)                                                       
         DC     AL2(0240)                                                       
         DC     AL2(0259)                                                       
         DC     AL2(0353)                                                       
         DC     AL2(0370)                                                       
         DC     AL2(0403)                                                       
         DC     AL2(0407)                                                       
         DC     AL2(0420)                                                       
         DC     AL2(0425)                                                       
         DC     AL2(0462)                                                       
         DC    X'0000'             END OF TABLE                                 
         EJECT                                                                  
SRTKMAJL EQU   ((INTKBTYP-INTKEY)+L'INTKBTYP)                                   
SRTKQHL  EQU   ((INTKQHS-INTKEY)+L'INTKQHS)                                     
SRTKWKL  EQU   ((INTKWKS-INTKEY)+L'INTKWKS)                                     
XMAR94   EQU   X'A1FC'             INVERTED MAR94                               
XMAY94   EQU   X'A1FA'             INVERTED MAY94                               
XOCT94   EQU   X'A1F5'             INVERTED OCT94                               
XNOV94   EQU   X'FFFF'-X'5E0B'     INVERTED NOV94                               
XJAN95   EQU   X'FFFF'-X'5F01'     INVERTED JAN95                               
XFEB95   EQU   X'FFFF'-X'5F02'     INVERTED FEB95                               
XMAR95   EQU   X'FFFF'-X'5F03'     INVERTED MAR95                               
XMAY95   EQU   X'FFFF'-X'5F05'     INVERTED MAY95                               
XJUL95   EQU   X'FFFF'-X'5F07'     INVERTED JUL95                               
XOCT95   EQU   X'FFFF'-X'5F0A'     INVERTED OCT95                               
XNOV95   EQU   X'FFFF'-X'5F0B'     INVERTED NOV95                               
XMAY96   EQU   X'FFFF'-X'6005'     INVERTED MAY96                               
                                                                                
DHOMELQ  EQU   X'33'               ELCODE OF ELEM CONTAINING DHOMES             
DHOMDSP  EQU   X'01'               POSITION OF DHOMES W/IN ELEMENT              
UHOMELQ  EQU   X'37'               ELCODE OF ELEM CONTAINING UHOMES             
UHOMDSP  EQU   X'00'               POSITION OF UHOMES W/IN ELEMENT              
***********************************************************************         
         SPACE 2                                                                
         DROP  R8,R9,RA,RB,RC                                                   
         EJECT                                                                  
***********************************************************************         
*======================= LOCAL WORKING STORAGE =======================*         
WORKD    DSECT                                                                  
NDXDA    DS    F                                                                
RELO     DS    F                                                                
COUNT    DS    F                                                                
DIRMAX   DS    F                   MAX NUMBER OF DIRECTORY READS                
ELCODE   DS    X                                                                
                                                                                
*                                 ************* ADDRESSES *************         
MYAIO    DS    A                   A(I/O AREA)                                  
MYAIOSRT DS    A                   A(I/O AREA FOR SORTER)                       
MYAQUART DS    A                   A(QUARTER-HOUR ELEMENT)                      
AMASKTAB DS    A                   A(MASKTAB)                                   
ADAYTAB  DS    A                   A(DAYTAB)                                    
                                                                                
*                                 ************** FILTERS **************         
FLTRMKT  DS    XL2                 RATING SERVICE MARKET                        
FLTBKY   DS    XL1                 BOOK YEAR                                    
FLTSTTN  DS    CL5                 STATION                                      
                                                                                
*                                 ********* THIS TIME'S VALUES ********         
THISSTTN DS    CL5                 STATION                                      
                                                                                
*                                 *********** PREVIOUS STUFF **********         
PREVDATA DS    0X                  PREVIOUS DATA                                
PRVSRTKY DS     0XL(INTKEYL)        PREVIOUS SORT KEY                           
PRVKSTTN DS      CL(L'INTKSTTN)      STATION                                    
PRVKKMKT DS      XL(L'INTKKMKT)      SPILL MARKET                               
PRVKBKY  DS      XL(L'INTKBKY)       BOOK YEAR                                  
PRVKSTYP DS      XL(L'INTKSTYP)      STATION TYPE                               
PRVKBTYP DS      XL(L'INTKBTYP)      BOOK TYPE                                  
PRVKDAY  DS      XL(L'INTKDAY)       DAY                                        
PRVKQHS  DS      XL(L'INTKQHS)       QUARTER HOURS                              
PRVKWKS  DS      XL(L'INTKWKS)       WEEKS                                      
PRVKELCD DS      XL(L'INTELCOD)      ELEMENT CODE                               
PRVKBKW  DS      XL(L'INTKBKW)       BOOK WEEK                                  
PREVDATX EQU   *                                                                
PREVDATL EQU   PREVDATX-PREVDATA                                                
                                                                                
CSTAT    DS    CL5                 CHAR STATION                                 
SVSTAT   DS    CL5                 SAVE CHAR STATION                            
                                                                                
*                                 *********** MISCELLANEOUS ***********         
DVDHOMES DS    F                   DHOMES DEMO VALUE                            
DVUHOMES DS    F                   UHOMES DEMO VALUE                            
DEMOVAL  DS    F                                                                
DEMOEL   DS    XL1                 ELCODE OF DEMO ELEMENT                       
DEMOPOS  DS    XL1                 POSITION OF DEMO                             
MTIMES   DS    0XL4                MILITARY TIMES                               
MSTIME   DS     XL2                 MILITARY START TIME                         
METIME   DS     XL2                 MILITARY END   TIME                         
                                                                                
*                                 ************** BUFFERS **************         
SPLKEY   DS    CL24                                                             
SVSPLKEY DS    CL(L'SPLKEY)                                                     
PREVKEY  DS    CL(L'SPLKEY)                                                     
TAPEBUFF DS    CL80                                                             
PREVBUFF DS    CL(L'TAPEBUFF)                                                   
                                                                                
X01ELEM  DS    XL(MARLNEQ)         X'01' ELEMENT                                
X20ELEM  DS    XL256               X'20' ELEMENT                                
X21ELEM  DS    XL256               X'21' ELEMENT                                
X51ELEM  DS    XL256               X'51' NEW ELEMENT FOR SPECIAL RECORD         
X53ELEM  DS    XL256               X'53' NEW ELEMENT FOR SPECIAL RECORD         
MYELEM   DS    XL256                                                            
                                                                                
IO       DS    2004X                                                            
IOL      EQU   *-IO                                                             
IOSRTR   DS    2004X                                                            
IOSRTRL  EQU   *-IOSRTR                                                         
                                                                                
*                                                                               
WORKX    EQU   *                                                                
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================= PRINTLINE =============================*         
PLINE    DSECT                                                                  
         DS    CL1                                                              
PLMNUM   DS    CL4                                                              
         DS    CL3                                                              
PLMNAME  DS    CL30                                                             
         DS    CL3                                                              
PLMRANK  DS    CL3                                                              
         DS    CL3                                                              
PLSRANK  DS    CL3                                                              
         DS    CL2                                                              
PLUSPCT  DS    CL6                                                              
         DS    CL1                                                              
PLHMSUNV DS    CL11                                                             
         SPACE 2                                                                
PSLINE   DSECT                                                                  
         DS    CL3                                                              
PSSTA    DS    CL4                                                              
         DS    CL2                                                              
PSMKTNUM DS    CL4                                                              
         DS    CL2                                                              
PSMKTNAM DS    CL30                                                             
         DS    CL4                                                              
PSBOOKS  DS    CL80                                                             
***********************************************************************         
         EJECT                                                                  
         PRINT OFF                                                              
***********************************************************************         
*============================ DEMO DSECTS ============================*         
*                                                                               
*------------------------------- DBLOCK ------------------------------*         
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
*----------------------------- DEDEMFILE -----------------------------*         
       ++INCLUDE DEDEMFILE                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================= DDCOMFACS =============================*         
       ++INCLUDE DDCOMFACS                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ SPOT DSECTS ============================*         
*                                                                               
*----------------------------- SPREPWORK -----------------------------*         
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
*                                                                               
*----------------------------- SPREPMODE -----------------------------*         
       ++INCLUDE SPREPMODES                                                     
***********************************************************************         
         EJECT                                                                  
         PRINT ON                                                               
***********************************************************************         
*=========================== INTERIM RECORD ==========================*         
                                                                                
INTRECD  DSECT                                                                  
INTRECLN DS    XL2                                                              
         DS    XL2                                                              
INTKEY   DS    0X                                                               
INTKSTTN DS     CL(L'DRSTAT)        STATION                                     
INTKKMKT DS     XL(L'DRKMKT)        SPILL MARKET                                
INTKBKY  DS     XL1                 DRBOOK+0(1)                                 
INTKSTYP DS     XL(L'DRSTYP)        STATION TYPE                                
INTKBTYP DS     XL(L'DRBTYP)        BOOK    TYPE                                
INTKDAY  DS     XL(L'QHDAY)         DAY                                         
INTKQHS  DS     XL(L'QHSQH+L'QHEQH) START/END QUARTER HOURS                     
INTKWKS  DS     XL(L'QHWKS)         WEEKS                                       
INTELCOD DS     XL1                 ELEMENT CODE                                
INTKBKW  DS     XL1                 DRBOOK+1(1)                                 
INTKEYX  EQU   *                                                                
INTKEYL  EQU   INTKEYX-INTKEY                                                   
INTFIXL  EQU   *-INTRECD                                                        
                                                                                
INTDATA  DS    0X                                                               
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPREPLR42 05/01/02'                                      
         END                                                                    

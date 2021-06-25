*          DATA SET CTMAD04A   AT LEVEL 053 AS OF 05/01/02                      
*PHASE TA0C04A,*                                                                
         TITLE 'TA0C04 - $MAD DOWNLOAD DEMOS'                                   
TA0C04   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C04,RA                                                      
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.  CURRENTLY, NOTHING NEEDS INITIALIZATION.                     
*                                                                               
INIT     NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE START MODE.  IT VALIDATES THE DEMO REQUEST         
* OBJECT PASSED BY THE PC AND RETURNS THE FIRST FRAME FULL OF DEMO              
* CELL OBJECTS.                                                                 
*                                                                               
PROCSTRT NTR1                                                                   
         XC    CURSTA,CURSTA       CLEAR CURRENT STATION                        
         XC    NUMOBJS,NUMOBJS           NUMBER OF DEMO OBJECTS                 
         MVI   UNITRANS,C'N'       SET UNIVERSE VALUES NOT TRANSFERED           
*                                                                               
*                                  OPEN TEMP FILE FOR PUTS                      
         GOTO1 TMPOPEN,DMCB,=C'PUT'                                             
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,TYPECDS          EXTRACT DEMTYPE ELCODES FROM DEMDISP         
*                                                                               
         BAS   RE,VALREQ           VALIDATE DEMO REQUEST OBJECT                 
*                                                                               
*                                  SWITCH TO SPOT SYSTEM/DEMFIL                 
         GOTO1 SETSYS,DMCB,=C'SPOT',0,=CL8'DEMFIL'                              
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,INITDB           INITIALIZE DBLOCK (DEMAND INTERFACE)         
*                                                                               
         BAS   RE,BLDSTA           BUILD STATION LIST                           
*                                                                               
         CLC   REQSTA,=CL5' '      IF SPECIFIC STATION REQUESTED                
         BNH   PS10                                                             
*                                                                               
         LA    R5,STALIST          THEN MAKE SURE STATION IS IN THE             
         L     R4,STATIONS             MARKET LIST                              
         CLC   REQSTA,0(R5)                                                     
         BE    PS10                                                             
         LA    R5,5(R5)                                                         
         BCT   R4,*-14                                                          
         B     ERRRQST             ERROR IF CAN'T FIND STATION                  
*                                                                               
PS10     BAS   RE,FILLTMP          FILL TEMPSTR WITH DEMOS                      
*                                                                               
         GOTO1 TMPCLOSE            CLOSE TEMP FILE                              
         BNE   EXIT                                                             
*                                                                               
         OC    NUMOBJS,NUMOBJS     IF NO DATA THEN ERROR                        
         BE    ERRNOSL                                                          
*                                                                               
         MVC   TMPTWA,CURRTWA      SAVE TWA NUMBER OF END OF TMPFILE            
         MVC   TMPDNEXT,DNEXTTMP        DISP WITHIN LAST TWA                    
*                                                                               
*                                  OPEN TEMP FILE FOR GETS                      
         GOTO1 TMPOPEN,DMCB,=C'GET',L'TMPAREA                                   
         BNE   EXIT                                                             
*                                                                               
         XC    TMPAREA,TMPAREA     CLEAR CONTINUE DATA                          
*                                                                               
         BAS   RE,FILLFRM          FILL FIRST FRAME WITH DEMO OBJECTS           
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE EXTRACTS THE ELEMENT CODES AND DATA ATTRIBUTES FROM              
* THE TABLE, DEMDISP, FOR IMPRESSIONS, UNIVERSE, AND CUMES DATA.                
*                                                                               
TYPECDS  NTR1                                                                   
         L     RF,DEMDISP          RF = A(DEMDISP'S FIRST SUBTABLE)             
         LA    RF,16(RF)                                                        
*                                                                               
TC10     CLC   0(3,RF),=C'TRA'     IF SUBTABLE 'TRA' THEN DONE                  
         BE    TC20                                                             
*                                                                               
         SR    RE,RE               ELSE BUMP RF TO NEXT SUBTABLE                
         ICM   RE,7,7(RF)                                                       
         AR    RF,RE                                                            
         LA    RF,1(RF)                                                         
*                                                                               
         CLI   0(RF),0             IF NOT END OF TABLE THEN LOOP BACK           
         BNE   TC10                                                             
         DC    H'0'                ELSE DIE                                     
*                                                                               
TC20     SR    RE,RE               RE = SIZE OF SUBTABLE ENTRY                  
         ICM   RE,3,5(RF)                                                       
         LA    RF,10(RF)           RF = A(FIRST SUBTABLE ENTRY)                 
*                                                                               
TC30     CLI   0(RF),C'I'          IF ENTRY FOR IMPRESSIONS                     
         BNE   TC40                                                             
         MVC   IELCODE,2(RF)       EXTRACT ELEMENT CODE FOR IMPRESSIONS         
         MVC   IATTR,4(RF)                 ATTRIBUTE BYTE                       
         B     TC90                                                             
*                                                                               
TC40     CLI   0(RF),C'U'          IF ENTRY FOR UNIVERSE                        
         BNE   TC50                                                             
         MVC   UELCODE,2(RF)       EXTRACT ELEMENT CODE FOR UNIVERSE            
         MVC   UATTR,4(RF)                 ATTRIBUTE BYTE                       
         B     TC90                                                             
*                                                                               
TC50     CLI   0(RF),C'C'          IF ENTRY FOR CUMES                           
         BNE   TC60                                                             
         MVC   CELCODE,2(RF)       EXTRACT ELEMENT CODE FOR CUMES               
         MVC   CATTR,4(RF)                 ATTRIBUTE BYTE                       
         B     TC90                                                             
*                                                                               
TC60     DS    0H                                                               
*                                                                               
TC90     AR    RF,RE               BUMP RF TO NEXT SUBTABLE ENTRY               
*                                                                               
         CLI   0(RF),X'FF'         IF NOT END OF SUBTABLE                       
         BNE   TC30                THEN LOOP BACK                               
*                                                                               
         MVI   TMPTYPE,ITRDATTR    PUT ATTR OBJECT TO TEMP FILE                 
         MVC   TMPIATTR,IATTR                                                   
         MVC   TMPUATTR,UATTR                                                   
         MVC   TMPCATTR,CATTR                                                   
         GOTO1 PUTTMP,DMCB,TMPAREA,TMPALEN                                      
         BNE   EXIT                                                             
*                                                                               
TCX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES MIDDLE MODE.  IT RETURNS THE NEXT FRAME FULL           
* OF DEMO CELL OBJECTS AND SETS THE LAST FRAME FLAG IF IT REACHES THE           
* END OF THE REQUESTED DEMOS.                                                   
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
         BAS   RE,FILLFRM          FILL FRAME WITH DEMO CELL OBJECTS            
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE GETS THE DEMO REQUEST OBJECT FROM THE INPUT FRAME AND            
* MAKES SURE IT IS VALID.                                                       
*                                                                               
VALREQ   NTR1                                                                   
         GOTO1 GETITEM             GET FIRST ITEM - FILTER OBJECT               
         BNE   EXIT                                                             
*                                  MUST BE TYPE DEMO REQUEST OBJECT             
         CLC   TYPENUM,=A(ITDEMREQ)                                             
         BNE   ERRRQCD                                                          
*                                  LENGTH MUST BE AT LEAST THE DISP TO          
         CLC   DATALEN,=A(REQUESTL)    DPT OR HOUR LIST                         
         BL    ERRRQLN                                                          
*                                                                               
         L     RE,ADATA            SAVE REQUEST IN REQUEST BLOCK                
         L     RF,DATALEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   REQUEST(0),0(RE)                                                 
*                                                                               
         LA    RE,REQUEST          RAISE REQUEST DATA TO UPPER CASE             
         L     RF,DATALEN                                                       
         OI    0(RE),X'40'                                                      
         LA    RE,1(RE)                                                         
         BCT   RF,*-8                                                           
*                                                                               
         CLI   REQMED,C'R'         MEDIA MUST BE RADIO                          
         BNE   ERRRQMD                                                          
*                                                                               
         CLI   REQSRC,C'A'         SOURCE MUST BE ARBITRON                      
         BNE   ERRRQSR                                                          
*                                  CONVERT HEX BOOK INTO LOCAL STORAGE          
         GOTO1 HEXIN,DMCB,REQBK,BOOK,4                                          
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRRQBK                                                          
*                                                                               
         MVI   BOOKTYPE,0          BOOK TYPE STANDARD IS ZERO                   
         CLI   REQBKTYP,C' '                                                    
         BE    VR10                                                             
         MVI   BOOKTYPE,C'B'       B FOR BLACK                                  
         CLI   REQBKTYP,C'B'                                                    
         BE    VR10                                                             
         MVI   BOOKTYPE,C'H'       H FOR HISPANIC                               
         CLI   REQBKTYP,C'H'                                                    
         BNE   ERRRQBT                                                          
*                                  MARKET CODE MUST BE NUMERIC                  
VR10     GOTO1 DECIN,DMCB,REQMK,4                                               
         BNE   ERRRQMK                                                          
*                                                                               
         PACK  DUB,REQMK           SAVE MARKET (2 BYTE BINARY) IN               
         CVB   R0,DUB                  LOCAL STORAGE                            
         STCM  R0,3,MARKET                                                      
*                                                                               
         LTR   R0,R0               PROTECT AGAINST MARKET NUMBER ZERO           
         BZ    ERRRQMK                                                          
*                                  VALIDATE GEOGRAPHIC AREA                     
         GOTO1 VALIRGEO,DMCB,REQGEOG                                            
         BNE   ERROBJ                                                           
*                                                                               
VR30     CLI   REQTYPE,C'D'        DATA TYPE MUST BE DAYPART OR HOURLY          
         BE    VR40                                                             
         CLI   REQTYPE,C'H'                                                     
         BNE   ERRRQDH                                                          
*                                  IF DPT OR HOUR LIST EMPTY THEN DONE          
VR40     CLC   DATALEN,=A(REQUESTL)                                             
         BE    VRX                                                              
*                                                                               
         LA    R1,4                ELSE LENGTH OF DPT OR HOUR LIST MUST         
         CLI   REQTYPE,C'D'            BE A MULTIBLE OF 4 IF DAYPART            
         BE    *+8                                                              
         LA    R1,10               AND 10 IF HOURLY                             
*                                                                               
         L     RF,DATALEN          DEVIDE LENGTH OF DPT OR HOUR LIST            
         S     RF,=A(REQUESTL)         BY APPROPRIATE MULTIBLE                  
         SR    RE,RE                                                            
         DR    RE,R1                                                            
         LTR   RE,RE                                                            
         BNZ   ERRRQLN             ERROR IF NOT EVEN MULTIBLE                   
*                                                                               
         L     R2,DATALEN          WHOLE LIST MUST BE VALID HEX                 
         S     R2,=A(REQUESTL)                                                  
         GOTO1 HEXIN,DMCB,REQDOH,AFREE,(R2)                                     
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRRQLN                                                          
*                                                                               
VRX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE INITIALIZES DBLOCK, THE DEMAND INTERFACE BLOCK, WITH             
* THE VALUES THAT ARE COMMON TO ALL THE DEMAND CALLS IN THIS OVERLAY.           
*                                                                               
INITDB   NTR1                                                                   
         XC    DBLOCK,DBLOCK       INITIALIZE DEMAND BLOCK                      
*                                                                               
         MVC   DBAREC,AIO          SET A(IOAREA)                                
         MVC   DBCOMFCS,ACOMFACS       A(COMFACS)                               
*                                                                               
         MVC   DBSELSRC,REQSRC     SET SOURCE (RATING SERVICE)                  
         MVC   DBSELMED,REQMED         MEDIA                                    
         MVC   DBSELAGY,SIGNON2C       AGENCY (2 BYTE CHARACTER)                
         MVC   DBSELBK,BOOK            BOOK                                     
         MVC   DBBTYPE,BOOKTYPE        BOOK TYPE                                
         MVC   DBSELRMK,MARKET         RATING SERVICE MARKET                    
         MVC   DBSELMK,MARKET          SAME FOR SPILL MARKET                    
*                                                                               
IDX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE BUILDS THE LIST OF STATIONS FOR THE SERVICE MARKET BY            
* CALLING DEMAND WHICH IN TURN HOOKS TO STAHOOK.                                
*                                                                               
BLDSTA   NTR1                                                                   
         XC    STATIONS,STATIONS   INITIALIZE NUMBER OF STATIONS                
*                                                                               
         MVC   DBFILE,=C'TP '      SET FILE TO 'TP ' FOR STATION LIST           
*                                                                               
         MVI   DBFUNCT,DBGETMS     FUNCTION = GET MKT STATION LIST              
*                                                                               
*                                  COMMENCE RECORD LOOP                         
         GOTO1 DEMAND,DMCB,DBLOCK,STAHOOK                                       
*                                                                               
         OC    STATIONS,STATIONS   IF NO STATIONS THEN MUST BE INVALID          
         BZ    ERRRQBK                 BOOK                                     
*                                                                               
BSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE IS HOOKED INTO BY DEMAND EVERY TIME DEMAND HAS A STATION         
* RECORD THAT WE ARE INTERESTED IN.  THE ROUTINE WILL EXTRACT THE DEMOS         
* IT NEEDS AND SAVE THEM IN TEMPSTR, AND THEN IT WILL RETURN TO DEMAND.         
*                                                                               
STAHOOK  NTR1                                                                   
         L     R6,AIO              R6 = A(DEMO RECORD)                          
         USING MLKEY,R6                                                         
*                                                                               
         L     RF,STATIONS         RF = A(NEXT STATION ENTRY)                   
         MH    RF,=H'5'                                                         
         LA    RF,STALIST(RF)                                                   
*                                                                               
         MVC   0(5,RF),MLSTAT      ADD STATION TO LIST                          
*                                                                               
         L     RF,STATIONS         INCREMENT NUMBER OF STATIONS                 
         LA    RF,1(RF)                                                         
         ST    RF,STATIONS                                                      
*                                                                               
SHX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE ADDS TO TEMPSTR THE DEMO RECORDS FOR THE STATION, DAYS,          
* TIMES, AND DAYPART NUMBER PASSED IN THE PARAMTERS.  IT CALLS DEMAND           
* WHICH IN TURN CALLS DEMHOOK WITH EACH RECORD WE'RE INTERESTED IN.             
*                                                                               
*        PARAMETER 1 - A(STATION CALL LETTERS)                                  
*        PARAMETER 2 - A(DAYS)                                                  
*        PARAMETER 3 - A(TIMES)                                                 
*        PARAMETER 4 - A(DAYPART NUMBER)                                        
*                                                                               
FILLTMP  NTR1                                                                   
         CLC   REQSTA,=CL5' '      IF SPECIFIC STATION REQUESTED                
         BNH   FT10                                                             
         GOTO1 CALLDEM,DMCB,REQSTA THEN CALL CALLDEM FOR THAT STATION           
         B     FT100                                                            
*                                                                               
FT10     LA    R3,STALIST          ELSE R3 = A(FIRST STATION IN LIST)           
         L     R5,STATIONS         R5 = NUMBER OF STATIONS IN LIST              
*                                                                               
FT50     GOTO1 CALLDEM,DMCB,(R3)   COMMENCE RECORD LOOP FOR STATION             
*                                                                               
         LA    R3,5(R3)            BUMP R3 TO NEXT STATION                      
         BCT   R5,FT50             REPEAT UNTIL NO MORE STATIONS                
*                                                                               
*                                  PUT EOD ITEM TO TEMP FILE                    
FT100    GOTO1 PUTTMP,DMCB,=X'FF',1                                             
         BNE   EXIT                                                             
*                                                                               
FTX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE CALLS DEMAND FOR THE STATION PASSED AND EACH DAYPART             
* OF HOUR IN THE REQDOH LIST.                                                   
*                                                                               
*        PARM 1 - A(STATION)                                                    
*                                                                               
CALLDEM  NTR1                                                                   
         BAS   RE,INITDB           INITIALIZE DBLOCK                            
*                                                                               
         MVI   DBFUNCT,DBGETDEM    SET FUNCTION = GET DEMOS                     
*                                                                               
         CLI   REQTYPE,C'D'        IF DAYPART DATA REQUESTED                    
         BNE   CD5                                                              
         MVC   DBFILE,=C'RDP'      THEN SET FILE TO 'RDP'                       
         B     CD7                                                              
*                                                                               
CD5      MVC   DBFILE,=C'TP '      ELSE SET FILE TO 'TP '                       
         MVI   DBSELDAY,X'07'      SET DAYS TO FRI-SUN                          
         MVC   DBSELTIM(2),=H'500' SET TIMES TO 5A-4A FOR ALL HOURS             
         MVC   DBSELTIM+2(2),=H'400'                                            
*                                                                               
CD7      L     R2,0(R1)            SET STATION IN DBLOCK FROM PARM 1            
         MVC   DBSELSTA,0(R2)                                                   
*                                  IF NO DPTS OR HOURS SPECIFED                 
         CLC   DATALEN,=A(REQUESTL)                                             
         BNE   CD10                                                             
*                                  THEN COMMENCE RECORD LOOP FOR ALL            
         GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK                                       
         B     CDX                                                              
*                                                                               
CD10     LA    R4,REQDOH           ELSE R4 = A(FIRST DPT OR HOUR)               
*                                                                               
         LA    RF,REQUEST          A(END OF REQUEST) = A(REQUEST) +             
         A     RF,DATALEN              LENGTH OF REQUEST                        
         ST    RF,AENDREQ                                                       
*                                                                               
CD50     C     R4,AENDREQ          WHILE NOT END OF DPT OR HOUR LIST            
         BNL   CDX                                                              
*                                                                               
         CLI   REQTYPE,C'D'        IF DAYPART DOWNLOAD                          
         BNE   CD60                THEN SET DAY IN DBLOCK                       
         GOTO1 CONVDAY,DMCB,0(R4),DBSELDAY                                      
         MVI   DBSELPRG,0          SET DAYPART IN DBLOCK                        
         GOTO1 HEXIN,DMCB,2(R4),DBSELPRG+1,2                                    
         B     CD70                                                             
*                                  ELSE SET DAYS/TIMES IN DBLOCK                
CD60     GOTO1 HEXIN,DMCB,0(R4),DBSELDAY,10                                     
*                                                                               
*                                  COMMENCE RECORD LOOP                         
CD70     GOTO1 DEMAND,DMCB,DBLOCK,DEMHOOK                                       
*                                                                               
         LA    R4,4(R4)            IF DPT DOWNLOAD THEN BUMP TO NEXT            
         CLI   REQTYPE,C'D'            DAYPART                                  
         BE    *+8                                                              
         LA    R4,6(R4)            ELSE BUMP TO NEXT HOUR                       
         B     CD50                LOOP BACK                                    
*                                                                               
CDX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE CONVERTS THE CHAR START AND END DATES PASSED IN PARM1            
* TO THE SPOT DAY BITS AND SAVES THE BITS AT THE ADDRESS POINTED TO             
* BY PARM2.                                                                     
*                                                                               
CONVDAY  NTR1                                                                   
         L     R2,0(R1)            R2 = A(CHAR START/END DAYS)                  
         MVC   FULL,4(R1)          FULL = DESTINATION ADDRESS                   
*                                                                               
*                                  CONVERT CHAR START/END DAYS TO BIN           
         GOTO1 HEXIN,DMCB,(R2),BYTE,2                                           
         NI    BYTE,X'7F'          ZERO OUT HIGH BIT                            
*                                                                               
         ZIC   R3,BYTE             R3 = START DAY (MON = 1, SUN = 7)            
         SRL   R3,4                                                             
         NI    BYTE,X'0F'          R4 = END DAY                                 
         ZIC   R4,BYTE                                                          
*                                                                               
         LA    R5,X'80'            SET R5 TO START DAY BIT                      
         LR    RF,R3                   (MON = X'40', SUN = X'01')               
*                                                                               
CV10     SRL   R5,1                SHIFT BIT RIGHT                              
         BCT   RF,CV10             UNTIL WE REACH THE START DAY                 
*                                                                               
         SR    R6,R6               SET START-END DAY BITS IN R6                 
*                                                                               
CV20     OR    R6,R5               SET BIT AND SHIFT RIGHT UNTIL                
         SRL   R5,1                    PAST END DAY                             
         LA    R3,1(R3)                                                         
         CR    R3,R4                                                            
         BNH   CV20                                                             
*                                                                               
         L     RF,FULL             SAVE BITS IN DESTINATION ADDRESS             
         STC   R6,0(RF)                                                         
*                                                                               
CVX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE IS HOOKED INTO BY DEMAND EVERY TIME DEMAND HAS A DEMO            
* RECORD THAT WE ARE INTERESTED IN.  THE ROUTINE WILL EXTRACT THE DEMOS         
* IT NEEDS AND SAVE THEM IN TEMPSTR, AND THEN IT WILL RETURN TO DEMAND.         
*                                                                               
DEMHOOK  NTR1                                                                   
         BAS   RE,TRACEDEM         TRACE DEMAND BLOCK IF DEBUG MODE             
         BE    DHX                                                              
*                                                                               
         CLC   DBACTSTA,CURSTA     IF STATION HAS CHANGED                       
         BE    DH5                                                              
*                                                                               
         MVI   TMPTYPE,ITRDSTA     THEN PUT STATION OBJECT TO TEMP FILE         
         MVC   TMPSTA,DBACTSTA                                                  
         GOTO1 PUTTMP,DMCB,TMPAREA,TMPSLEN                                      
         BNE   EXIT                                                             
*                                                                               
         MVC   CURSTA,DBACTSTA     SAVE CURRENT STATION                         
         MVI   DUPTAB,0            CLEAR DUPLCATE DAY/DAYPARTS TABLE            
*                                                                               
DH5      CLI   REQTYPE,C'D'        IF REQUEST ID FOR DAYPART DATA               
         BNE   DH10                                                             
*                                  THEN EXTRACT DPT INFO FROM DBLOCK            
         GOTO1 DEFINE,DMCB,=C'DPT',DBLOCK,BLOCK                                 
*                                                                               
         BAS   RE,TESTDUP          SKIP THIS RECORD IF DUPLICATE                
         BE    DHX                                                              
*                                                                               
         MVI   TMPTYPE,ITRDDPT     PUT DAYPART OBJECT TO TEMP FILE              
         MVC   TMPDDAY,BLOCK                                                    
         MVC   TMPDDPT,BLOCK+1                                                  
         GOTO1 GETELEM,DMCB,MARCODEQ                                            
         USING MARELEM,R6                                                       
         MVI   TMPDMTYP,0          SET MARKET TYPE TO STANDARD                  
         CLI   MARTYPE,C'C'        TEST FOR CONDENSED                           
         BNE   *+8                                                              
         MVI   TMPDMTYP,1          YES                                          
         MVC   TMPDSVIO,MARELEM+9                                               
         CLI   TMPDSVIO,C' '       BLANK IS LOWEST STATION                      
         BH    *+8                 VIOLATION VALUE                              
         MVI   TMPDSVIO,C' '                                                    
         GOTO1 PUTTMP,DMCB,TMPAREA,TMPDLEN                                      
         BNE   EXIT                                                             
         B     DH20                                                             
         DROP  R6                                                               
*                                  ELSE EXTRACT DAY INFO FROM DBLOCK            
DH10     GOTO1 DEFINE,DMCB,=C'DAY',DBLOCK,BLOCK                                 
*                                                                               
*                                  EXTRACT TIME INFO FROM DBLOCK                
         GOTO1 DEFINE,DMCB,=C'TIME',DBLOCK,ELEMENT                              
*                                                                               
         MVI   TMPTYPE,ITRDHRS     PUT HOURS OBJECT TO TEMP FILE                
         MVC   TMPHDAY,BLOCK                                                    
         MVC   TMPHSQH,ELEMENT                                                  
         MVC   TMPHEQH,ELEMENT+1                                                
         GOTO1 GETELEM,DMCB,MARCODEQ                                            
         USING MARELEM,R6                                                       
         MVI   TMPHMTYP,0          SET MARKET TYPE TO STANDARD                  
         CLI   MARTYPE,C'C'        TEST FOR CONDENSED                           
         BNE   *+8                                                              
         MVI   TMPHMTYP,1          YES                                          
         MVC   TMPHSVIO,MARELEM+9                                               
         CLI   TMPHSVIO,C' '       BLANK IS LOWEST STATION                      
         BH    *+8                 VIOLATION VALUE                              
         MVI   TMPHSVIO,C' '                                                    
         GOTO1 PUTTMP,DMCB,TMPAREA,TMPHLEN                                      
         BNE   EXIT                                                             
         DROP  R6                                                               
*                                                                               
DH20     ST    R6,DUB              SAVE R6=A(1ST SECTN LEAD ELEM)               
         PRINT GEN                                                              
         GOTO1 GETELEM,DMCB,X5E    GET BOOK FOR DEMO ELEMS OFF X'5E'            
         BNE   *+10                                                             
         MVC   BK5E(2),5(R6)       SAVE OFF FOR LATER PROC DEMOS                
         L     R6,DUB              RESTORE PTR                                  
*--MTA                             R6 = A(FIRST SECTION LEAD ELEMENT)           
         XC    DCH0,DCH0                                                        
         B     DH25                THIS CHUNK IS FOR TESING ONLY                
         L     R1,AIO                                                           
         CLC   0(8,R1),=C'DRAWABCA'                                             
         BNE   DH25                                                             
         GOTO1 GETELEM,DMCB,X20    GET BOOK FOR DEMO ELEMS OFF X'5E'            
         CLC   6(10,R6),=C'M-F 10A-3P'                                          
         BNE   *+8                                                              
         MVI   DCH0,X'FF'                                                       
*--MTA                             R6 = A(FIRST SECTION LEAD ELEMENT)           
DH25     DS    0H                                                               
         L     R6,DUB              RESTORE PTR                                  
         GOTO1 GETELEM,DMCB,SLCODEQ                                             
         USING SLELEM,R6                                                        
         PRINT NOGEN                                                            
*                                                                               
DH30     BNE   DHX                 IF END OF RECORD REACHED THEN NONE           
*                                                                               
         CLC   SLSECT,BRADGEOG     IF GEOG MATCHES ELEM THEN DONE               
         BE    DH40                                                             
         GOTO1 NEXTELEM            ELSE GET NEXT SL ELEM AND LOOP BACK          
         B     DH30                                                             
*                                                                               
DH40     MVI   CMELCODE,0          LOOP THROUGH DEMO VALUES ELEMENTS            
*                                                                               
DH50     GOTO1 NEXTELEM            GET NEXT ELEMENT                             
         BNE   DHX                 IF END OF DEMO VALUES ELEMENTS               
         CLI   0(R6),X'5E'                                                      
         BNL   DHX                 THEN DONE                                    
*                                                                               
         BAS   RE,TESTVAL          PUT VALUES ITEM IF CORRECT ELEMENT           
         B     DH50                LOOP BACK                                    
*                                                                               
DHX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE TRACES THE DEMAND BLOCK TO TEMPFILE IF PCDEBUG IS 'Y'.           
*                                                                               
TRACEDEM NTR1                                                                   
         CLI   PCDEBUG,C'Y'                                                     
         BNE   NO                                                               
         GOTO1 HEXOUT,DMCB,DBSELECT,TMPAREA,64                                  
         GOTO1 PUTTMP,DMCB,TMPAREA,128                                          
         BNE   EXIT                                                             
         GOTO1 HEXOUT,DMCB,DBACTUAL,TMPAREA,64                                  
         GOTO1 PUTTMP,DMCB,TMPAREA,128                                          
         BNE   EXIT                                                             
         GOTO1 HEXOUT,DMCB,AIO,TMPAREA,20                                       
         GOTO1 PUTTMP,DMCB,TMPAREA,40                                           
         BNE   EXIT                                                             
         B     YES                                                              
         EJECT                                                                  
* THIS ROUTINE TESTS IF THE DAY/DAYPART PAIR FOUND IN BLOCK IS A                
* DUPLICATE BY LOOKING FOR IT IN DUPTAB.  IF IT IS THERE, THE ROUTINE           
* RETURNS 'YES'.  OTHERWISE, THE ROUTINE ADDS THE PAIR TO THE TABLE             
* AND RETURNS 'NO'.                                                             
*                                                                               
TESTDUP  NTR1                                                                   
         LA    R2,DUPTAB           R2 = A(DUPLICATE DAY/DPTS TABLE)             
*                                                                               
TD10     CLI   0(R2),0             IF END OF TABLE THEN NO DUPLICATE            
         BE    TD20                                                             
         CLC   0(2,R2),BLOCK       ELSE IF PAIR MATCHES THEN DUPLICATE          
         BE    TDYES                                                            
         LA    R2,2(R2)            ELSE BUMP TO NEXT TABLE ENTRY                
         B     TD10                                                             
*                                                                               
TD20     MVC   0(2,R2),BLOCK       ADD CURRENT PAIR TO TABLE                    
         MVI   2(R2),0                                                          
*                                                                               
TDNO     B     NO                                                               
*                                                                               
TDYES    B     YES                                                              
         EJECT                                                                  
* THIS ROUTINE DETERMINES IF THE DEMO VALUES ELEMENT POINTED TO BY R6           
* IS TO BE DOWNLOADED TO THE PC, AND IF SO, IT SAVES THE ELEMENT'S              
* DATA IN THE TEMP FILE.                                                        
*                                                                               
TESTVAL  NTR1                                                                   
         CLC   IELCODE,0(R6)       IF VALUES ELEMENT IS FOR IMPRESSIONS         
         BNE   TV10                                                             
         MVI   TMPTYPE,ITRDVIMP    THEN SET ITEM TYPE TO IMPRESSIONS            
         B     TV50                                                             
*                                                                               
         CLC   IELCODE,0(R6)       IF VALUES ELEMENT IS FOR IMPRESSIONS         
         BNE   TV10                                                             
         MVI   TMPTYPE,ITRDVIMP    THEN SET ITEM TYPE TO IMPRESSIONS            
         B     TV50                                                             
*                                                                               
TV10     CLC   UELCODE,0(R6)       ELSE IF VALUES ELEMENT IS UNIVERSE           
         BNE   TV20                                                             
         CLI   UNITRANS,C'Y'       AND UNIVERSE DATA NOT YET TRANSFERED         
         BE    TVX                                                              
         MVI   TMPTYPE,ITRDVUNI    THEN SET ITEM TYPE TO UNIVERSE               
         MVI   UNITRANS,C'Y'       SET UNIVERSE DATA TRANSFERED                 
         B     TV50                                                             
*                                                                               
TV20     CLC   CELCODE,0(R6)       ELSE IF VALUES ELEMENT IS FOR CUMES          
         BNE   TVX                                                              
         MVI   TMPTYPE,ITRDVCUM    THEN SET ITEM TYPE TO CUMES                  
*                                                                               
TV50     DS    0H                                                               
         CLC   BK5E,=X'6002'       POST 9602 DEMOS DIFFER FROM OLD              
         BL    *+12                                                             
         BAS   RE,RESLOT           CONVERT NEW DEMS TO OLD DEMS                 
         B     TV55                                                             
         ZIC   R2,1(R6)            MOVE ATTRIBUTE AND VALUES TO TMPAREA         
         SH    R2,=H'3'                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   TMPVATTR,2(R6)                                                   
         LA    R2,2(R2)            PUT TMPAREA TO TEMP FILE                     
*                                                                               
TV55     DS    0H                                                               
         GOTO1 PUTTMP,DMCB,TMPAREA,(R2)                                         
         BNE   EXIT                                                             
*                                                                               
         L     RF,NUMOBJS          INCREMENT NUMBER OF OBJECTS PUT              
         LA    RF,1(RF)                                                         
         ST    RF,NUMOBJS                                                       
*                                                                               
TVX      B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* RESLOT - THIS ROUTINE CALCULATES THE OLD DEMO CELLS FROM THE DEMO             
*          CELLS SAVED ON THE RECDS AFTER SEP/95. DEMOS ARE RE-SLOTTED          
*          AND CONDENSED INTO TMPVATTR (AS IF THEY CAME IN THAT WAY).           
*          INPUT:  0(R6)    - PTS TO NEW DEMO ELEMENT IN RECD                   
*          OUTPUT: TMPVATTR - LIST OF CONDENSED OLD DEMOS                       
*                  R2       = LENGTH OF DEMOS + 1BYTE PREC IN TMPVATTR          
**********************************************************************          
*                                                                               
RESLOT   NTR1                                                                   
         XC    NEWDEMS,NEWDEMS     INIT NEWDEMS BUCKET                          
         XC    OLDDEMS,OLDDEMS     INIT OLDEMS BUCKET                           
         ZIC   R0,1(R6)                                                         
         AR    R0,R6                                                            
         ST    R0,DMCB             ADDR OF END OF ELEM                          
         MVC   DMCB+4(1),2(R6)     SAVE PRECISION LO ORDER NIBBLE               
         MVC   DMCB+5(1),2(R6)     SAVE HI ORDER NIBBLE                         
         NI    DMCB+4,X'0F'                                                     
         ZIC   R0,DMCB+4           LENGTH OF EACH FLD IN ELEMT                  
         LA    R1,4                                                             
         SR    R1,R0               #BYTES TO DISP INTO BUFFER                   
         LA    R5,NEWDEMS          SLOT INTO BEGIN SLOT SPECIFIED               
         AR    R5,R1                                                            
         LA    R6,3(R6)            PT TO 1ST DEMO IN ELEMENT                    
*                                                                               
RESL20   ZIC   R1,DMCB+4           PRECISION                                    
         C     R6,DMCB             END OF THIS ELELMENT                         
         BNL   RESL30                                                           
*                                                                               
RESL25   BCTR  R1,0                MOVE DEMO INTO SLOT                          
         EXMVC R1,0(R5),0(R6)                                                   
         LA    R5,4(R5)            BUMP UNIV PTR                                
         AR    R6,R1               BUMP ELEMENT PTR                             
         LA    R6,1(R6)            +1 FROM BCTR                                 
         B     RESL20                                                           
*                                                                               
RESL30   DS    0H                  NOW PROCESS DEMOS                            
         LA    R6,NEWDEMS          START OF NEW DEMOS                           
         USING NFMT,R6                                                          
         LA    R5,OLDDEMS          WHERE TO STORE COMPUTED OLD DEMOS            
         USING OFMT,R5                                                          
         MVC   OM1824,NM1824       M1824    - STRAIGHT TRANSFERS                
         MVC   OM2534,NM2534       M2534                                        
         MVC   OM3544,NM3544       M3544                                        
         MVC   OM5564,NM5564       M5564                                        
         MVC   OM65P,NM65P         M65P                                         
         MVC   OW1824,NW1824       W1824                                        
         MVC   OW2534,NW2534       W2534                                        
         MVC   OW3544,NW3544       W3544                                        
         MVC   OW5564,NW5564       W5564                                        
         MVC   OW65P,NW65P         W65P                                         
*                                                                               
         ICM   R1,15,NM2534        M2549 = M2534+M3544+M4549                    
         A     R1,NM3544                                                        
         A     R1,NM4549                                                        
         STCM  R1,15,OM2549                                                     
*                                                                               
         A     R1,NM1824           M1849 = M2549+M4549                          
         STCM  R1,15,OM1849                                                     
*                                                                               
         A     R1,NM5054           M18+ = M1849 + M5054 + M5564 +M65+           
         A     R1,NM5564                                                        
         A     R1,NM65P                                                         
         STCM  R1,15,OM18P                                                      
*                                                                               
         ICM   R1,15,NM4549        M4554 = M4549+M5054                          
         A     R1,NM5054                                                        
         STCM  R1,15,OM4554                                                     
*                                                                               
         ICM   R1,15,NW2534        W2549 = W2534+W3544+W4549                    
         A     R1,NW3544                                                        
         A     R1,NW4549                                                        
         STCM  R1,15,OW2549                                                     
*                                                                               
         A     R1,NW1824           W1849 = W2549+W4549                          
         STCM  R1,15,OW1849                                                     
*                                                                               
         A     R1,NW5054           W18+ = W1849 + W5054 + W5564 +W65+           
         A     R1,NW5564                                                        
         A     R1,NW65P                                                         
         STCM  R1,15,OW18P                                                      
*                                                                               
         ICM   R1,15,NW4549        W4554 = W4549+W5054                          
         A     R1,NW5054                                                        
         STCM  R1,15,OW4554                                                     
*                                                                               
         ICM   R1,15,NW1217        W1224 = W1217 + W1824                        
         A     R1,NW1824                                                        
         STCM  R1,15,OW1224                                                     
*                                                                               
         ICM   R1,15,NM1217        V1217 = M1217 + W1217                        
         A     R1,NW1217                                                        
         STCM  R1,15,OV1217                                                     
*                                                                               
         A     R1,OM18P            V12+ = V1217 + W18P + M18P                   
         A     R1,OW18P                                                         
         STCM  R1,15,OV12P                                                      
*--DIVIDE BY 100 (TRUNCATING) AND SAVE LARGEST VALUE IN DUB                     
         LA    RE,OLDDEMSQ         R5=OLDEMS, RE=#DEMOS IN BUFFER               
         XC    DUB,DUB             RE=LARGEST VALUE SAVED IN ELEMENT            
RESL40   SR    R0,R0                                                            
         ICM   R1,15,0(R5)         PICK UP VALUE                                
         TM    DMCB+5,X'40'           DO WE NEED TO MULT BY 10?                 
         BO    *+8                                                              
         M     R0,=F'10'           YES                                          
         A     R1,=F'50'           ROUND DEMOS                                  
         SR    R0,R0                                                            
         D     R0,=F'100'          ADJUST PRECISION                             
         STCM  R1,15,0(R5)         REPLACE IN BUCKET                            
         C     R1,DUB              SAVE LARGEST VALUE                           
         BL    *+8                                                              
         ST    R1,DUB              RE=LARGEST DEMO VALUE                        
         LA    R5,4(R5)            BUMP TO NEXT BUCKET                          
         BCT   RE,RESL40                                                        
*                                                                               
         ICM   R1,15,DUB           SEE IF WE HAVE NUMBERS LEFT                  
         LTR   R1,R1                                                            
         BNZ   RESL43              YES, GO PUT DEMOS IN TMPVATTR                
         XC    TMPVATTR(255),TMPVATTR                                           
         MVI   TMPVATTR,X'41'      DUMMY UP, SET TO 1BYTE FIELD                 
         LA    R2,2                LENGTH OF BUFFER (X'4100' = 2BYTES)          
         B     RESLX               EXIT                                         
*                                                                               
RESL43   LA    R1,4                R0=# BYTES OF LRGEST DEMO BUCKET             
         LA    RF,DUB                                                           
         MVI   DUB+4,0                                                          
RESL45   CLI   0(RF),0                                                          
         BNE   *+12                                                             
         LA    RF,1(RF)                                                         
         BCT   R1,RESL45                                                        
*                                                                               
         XC    TMPVATTR(255),TMPVATTR                                           
         STC   R1,TMPVATTR         SAVE PRECISION                               
         OI    TMPVATTR,X'40'      MULT OCCURED IF IT WAS NEEDED                
         LA    RE,TMPVATTR+1       RE=DEST TO STORE DEMOS                       
         LA    R5,OLDDEMS                                                       
         LA    R0,4                                                             
         SR    R0,R1               DISP AMT INTO OLDDEMS  BUFFER                
         AR    R5,R0               R5=SOURCE IN OLDDEMS                         
         BCTR  R1,0                #BYTES TO MOVE -1 FOR EXMVC                  
*                                                                               
         LA    R0,OLDDEMSQ         MAX # DEMOS                                  
RESL50   DS    0H                                                               
         EXMVC R1,0(RE),0(R5)      MOVE IN & CONDENSE DEMO                      
         EX    R1,*+8              EX OR W/ITSELF TO SEE IF ZERO                
         B     *+10                                                             
         OC    0(0,RE),0(RE)                                                    
         BZ    *+8                                                              
         ST    RE,DUB              SAVE ADDR OF LAST BUCKET W/VALUE             
         LA    R5,4(R5)            BUMP OLDEMS PTR                              
         LA    RE,1(R1,RE)         BUMP OUTPUT PTR                              
         BCT   R0,RESL50                                                        
*                                                                               
         LA    RE,TMPVATTR         START OF OUTPUT                              
         L     R2,DUB              A(LAST NON-ZERO DEMO)                        
         LA    R2,1(R1,R2)         BUMP TO END OF FIELD                         
         SR    R2,RE               PREC FIELD + DEMOS                           
         LA    R2,1(R2)            +1                                           
*                                                                               
RESLX    DS    0H                                                               
         CLI   DCH0,X'FF'                                                       
         B     *+14                FOR TESTING ONLY                             
         LA    R5,NEWDEMS                                                       
         LA    R6,OLDDEMS                                                       
         DC    H'0'                                                             
*                                                                               
         XIT1  REGS=(R2)                                                        
         DROP  R5,R6                                                            
*                                                                               
         EJECT                                                                  
* ********************************************************************          
* THIS ROUTINE FILLS THE FRAME WITH DEMO OBJECTS.                               
* ********************************************************************          
*                                                                               
FILLFRM  NTR1                                                                   
         OC    TMPAREA,TMPAREA     IF CONTINUE TMPAREA EXISTS THEN CONT         
         BNZ   FF20                                                             
*                                  GET NEXT ITEM FROM TEMP FILE                 
FF10     GOTO1 GETTMP,DMCB,TMPAREA                                              
         BNE   EXIT                                                             
         MVC   TMPLEN,4(R1)        SAVE LENGTH OF ITEM IN TMPLEN                
*                                                                               
         CLI   EOTFLAG,C'Y'        IF END OF TEMP FILE                          
         BNE   FF20                                                             
*                                                                               
         GOTO1 TMPCLOSE            THEN CLOSE TEMP FILE                         
         BNE   EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR AND DONE            
         B     FF100                                                            
*                                                                               
FF20     CLI   TMPTYPE,ITRDSTA     IF ITEM TYPE IS STATION                      
         BNE   FF30                THEN MOVE STATION TO BLOCK                   
         MVC   BLOCK(TMPSLEN-1),TMPDATA                                         
         LA    R2,TMPSLEN-1        SET ITEM LENGTH TO STATION LENGTH            
         B     FF40                                                             
*                                                                               
FF30     L     R2,TMPLEN           ELSE CONVERT ITEM DATA HEX TO CHAR           
         BCTR  R2,0                                                             
         MVI   BYTE,C'N'           SET DPT/HRS OBJECT FLAG                      
         CLI   TMPTYPE,ITRDDPT     TEST DPT/HRS OBJECT                          
         BE    FF32                YES                                          
         CLI   TMPTYPE,ITRDHRS                                                  
         BNE   FF35                                                             
*                                                                               
FF32     MVI   BYTE,C'Y'           SET FLAG ACCORDINGLY                         
         BCTR  R2,0                LAST BYTE IS CHARACTER DATA                  
*                                                                               
FF35     GOTO1 HEXOUT,DMCB,TMPDATA,BLOCK,(R2)                                   
         CLI   BYTE,C'Y'           TEST DPT/HRS OBJECT                          
         BNE   FF38                                                             
*                                                                               
         LA    RE,TMPDATA(R2)      YES-POINT TO LAST DATA BYTE                  
         SLL   R2,1                COMPUTE LENGTH OF OUTPUT                     
         LA    R1,BLOCK(R2)        INDEX INTO OUTPUT BLOCK                      
         MVC   0(1,R1),0(RE)       COPY STATION VIOLATION CHAR                  
         LA    R2,1(R2)            INCREMENT OUTPUT LENGTH                      
         B     FF40                                                             
*                                                                               
FF38     SLL   R2,1                SET ITEM LENGTH TO CHAR LENGTH               
*                                                                               
FF40     CLI   TMPTYPE,X'FF'       IF TMPTYPE IS X'FF'                          
         BNE   FF50                THEN PUT EOD ITEM TO OUTPUT FRAME            
         GOTO1 PUTITEM,DMCB,999,0,BLOCK                                         
         BNE   EXIT                                                             
         B     FF90                                                             
*                                                                               
FF50     ZIC   R3,TMPTYPE          ELSE PUT GOALS ITEM TO OUTPUT FRAME          
         GOTO1 PUTITEM,DMCB,(R3),(R2),BLOCK                                     
         BNE   EXIT                                                             
*                                                                               
FF90     CLI   EOFFLAG,C'Y'        IF NOT END OF FRAME                          
         BNE   FF10                THEN GO BACK                                 
*                                                                               
FF100    L     RF,TMPTWA           RF = NUMBER OF TEMP FILE BUFFERS             
         S     RF,CURRTWA              REMAINING (SKIP TWA5)                    
         CLC   TMPTWA,=F'5'                                                     
         BL    FF110                                                            
         CLC   CURRTWA,=F'5'                                                    
         BH    FF110                                                            
         BCTR  RF,0                                                             
*                                                                               
FF110    MH    RF,LENTWA           RF = NUMBER OF BYTES REMAINING IN            
         A     RF,TMPDNEXT              TEMP FILE                               
         S     RF,DNEXTTMP            = (# BUFFERS LEFT) * LENTWA +             
         S     RF,=F'2'                 (TMPDNEXT - DNEXTTMP) - 2               
*                                                                               
         MVC   FULL,=F'850'        FULL = NUMBER OF BYTES PER FRAME             
         CLC   PCVRS,=H'310'            = 850 IF VERSION < 310                  
         BL    *+10                     = 900 OTHERWISE                         
         MVC   FULL,=F'900'                                                     
*                                                                               
         SR    RE,RE               RF = EST NUMBER OF FRAMES REMAINING          
         D     RE,FULL                = TEMP FILE BYTES / BYTES PER FRM         
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
*                                                                               
         GOTO1 ESTFRM,DMCB,(RF)    SET NEW ESTIMATD TOTAL FRAMES                
*                                                                               
FFX      B     XIT                                                              
         EJECT                                                                  
*              INVALID REQUEST CODE                                             
ERRRQCD  MVC   APPLERR,=Y(ER04RQCD)                                             
         B     ERROBJ                                                           
*              INVALID REQUEST LENGTH                                           
ERRRQLN  MVC   APPLERR,=Y(ER04RQLN)                                             
         B     ERROBJ                                                           
*              INVALID REQUEST MEDIA                                            
ERRRQMD  MVC   APPLERR,=Y(ER04RQMD)                                             
         B     ERROBJ                                                           
*              INVALID REQUEST RATING SERVICE                                   
ERRRQSR  MVC   APPLERR,=Y(ER04RQSR)                                             
         B     ERROBJ                                                           
*              INVALID REQUEST BOOK                                             
ERRRQBK  MVC   APPLERR,=Y(ER04RQBK)                                             
         B     ERROBJ                                                           
*              INVALID REQUEST BOOK TYPE                                        
ERRRQBT  MVC   APPLERR,=Y(ER04RQBT)                                             
         B     ERROBJ                                                           
*              INVALID REQUEST MARKET                                           
ERRRQMK  MVC   APPLERR,=Y(ER04RQMK)                                             
         B     ERROBJ                                                           
*              INVALID REQUEST GEOGRAPHIC AREA                                  
ERRRQGE  MVC   APPLERR,=Y(ER04RQGE)                                             
         B     ERROBJ                                                           
*              INVALID REQUEST DAYPART/HOUR INDICATOR                           
ERRRQDH  MVC   APPLERR,=Y(ER04RQDH)                                             
         B     ERROBJ                                                           
*              INVALID REQUEST HEX DAYPART/HOUR LIST                            
ERRRQHX  MVC   APPLERR,=Y(ER04RQHX)                                             
         B     ERROBJ                                                           
*              INVALID REQUEST STATION                                          
ERRRQST  MVC   APPLERR,=Y(ER04RQST)                                             
         B     ERROBJ                                                           
*              NO DEMO DATA                                                     
ERRNOSL  MVC   APPLERR,=Y(ER04NOSL)                                             
         B     ERROBJ                                                           
*                                  RETURN APPL ERRORS IN ERR OBJECT             
ERROBJ   GOTO1 HEXOUT,DMCB,APPLERR,FULL,2                                       
         GOTO1 PUTITEM,DMCB,ITAMFMER,4,FULL                                     
         BNE   EXIT                                                             
*                                  PUT END OF DATA ITEM                         
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
*                                                                               
EXIT     L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* DEDEMFILE                                                                     
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
*                                                                               
X5E      EQU   X'5E'                                                            
X20      EQU   X'20'                                                            
*---------------------------------------------------------------------          
*OFMT/NFMT - TO CONVERT FROM NEW DEMOS TO OLD STYLE DEMOS                       
*---------------------------------------------------------------------          
OFMT     DSECT                    OLD FORMAT DEMOS                              
OV12P    DS    XL4                                                              
OM18P    DS    XL4                                                              
OM1824   DS    XL4                                                              
OM1849   DS    XL4                                                              
OM2534   DS    XL4                                                              
OM2549   DS    XL4                                                              
OM3544   DS    XL4                                                              
OM4554   DS    XL4                                                              
OM5564   DS    XL4                                                              
OM65P    DS    XL4                                                              
OW18P    DS    XL4                                                              
OW1824   DS    XL4                                                              
OW1849   DS    XL4                                                              
OW2534   DS    XL4                                                              
OW2549   DS    XL4                                                              
OW3544   DS    XL4                                                              
OW4554   DS    XL4                                                              
OW5564   DS    XL4                                                              
OW65P    DS    XL4                                                              
OW1224   DS    XL4                                                              
OV1217   DS    XL4                                                              
OLDDEMSQ EQU   (*-OV12P)/4                                                      
         SPACE 2                                                                
*                                                                               
NFMT     DSECT                                                                  
NM1217   DS    XL4                                                              
NM1824   DS    XL4                                                              
NM2534   DS    XL4                                                              
NM3544   DS    XL4                                                              
NM4549   DS    XL4                                                              
NM5054   DS    XL4                                                              
NM5564   DS    XL4                                                              
NM65P    DS    XL4                                                              
NW1217   DS    XL4                                                              
NW1824   DS    XL4                                                              
NW2534   DS    XL4                                                              
NW3544   DS    XL4                                                              
NW4549   DS    XL4                                                              
NW5054   DS    XL4                                                              
NW5564   DS    XL4                                                              
NW65P    DS    XL4                                                              
NEWDEMSQ EQU   (*-NM1217)/4            NUMBER OF DEMOS                          
         EJECT                                                                  
*                                                                               
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
***********************************************************************         
*        VARIABLES EXTRACTED FROM DEMDISP                                       
***********************************************************************         
IELCODE  DS    C                   IMPRESSIONS ELEMENT CODE                     
IATTR    DS    C                   IMPRESSIONS ATTRIBUTE BYTE                   
UELCODE  DS    C                   UNIVERSE ELEMENT CODE                        
UATTR    DS    C                   UNIVERSE ATTRIBUTE BYTE                      
CELCODE  DS    C                   CUME ELEMENT CODE                            
CATTR    DS    C                   CUME ATTRIBUTE BYTE                          
***********************************************************************         
*        REQUEST OBJECT                                                         
***********************************************************************         
REQUEST  DS    0C                  PC REQUEST OBJECT DATA                       
REQMED   DS    C                   MEDIA                                        
REQSRC   DS    C                   SOURCE (RATING SERVICE)                      
REQBK    DS    CL4                 BOOK                                         
REQBKTYP DS    C                   BOOK TYPE                                    
REQMK    DS    CL4                 MARKET                                       
REQSTA   DS    CL5                 STATION                                      
REQGEOG  DS    C                   GEOGRAPHIC AREA                              
REQTYPE  DS    C                   TYPE OF DATA (D)AYPART/(H)OURLY              
REQUESTL EQU   *-REQUEST                                                        
REQDOH   DS    0C                  DPT OR HOUR LIST (NONE MEANS ALL)            
*                                  DATA IS CHAR FORM OF HEX VALUES              
*                                  IF REQTYPE = 'D' THEN EACH ELEMENT           
*                                      IN THE LIST IS DAYS (2 BYTES)/           
*                                      DAYPART NUMBER (2 BYTES)                 
*                                  IF REQTYPE = 'H' THEN EACH ELEMENT           
*                                      IN THE LIST IS DAYS (2 BYTES)/           
*                                      START/END TIMES (8 BYTES)                
         DS    600C                LEAVE ROOM FOR 60 HOURS                      
AENDREQ  DS    A                   A(END OF REQUEST)                            
***********************************************************************         
*        HEX FORM OF NUMERIC REQUEST OBJECT DATA                                
***********************************************************************         
BOOK     DS    XL2                 BOOK                                         
BOOKTYPE DS    X                   BOOK TYPE                                    
MARKET   DS    XL2                 MARKET                                       
***********************************************************************         
*        VARIABLES CREATED IN STAHOOK                                           
***********************************************************************         
STATIONS DS    F                   NUMBER OF STATIONS IN MARKET                 
STALIST  DS    500C                LIST OF STATIONS IN MARKET                   
***********************************************************************         
*        TEMP FILE ITEM WORK AREA                                               
*        ALL TYPES OF ITEM DATA ARE ORGED OVER EACH OTHER                       
***********************************************************************         
TMPLEN   DS    F                   LENGTH OF TMPAREA (FROM GETTMP)              
TMPAREA  DS    0XL256              GETTMP/PUTTMP WORK AREA                      
*                                                                               
TMPTYPE  DS    X                   TEMP FILE ITEM TYPE                          
TMPDATA  DS    0X                  TEMP FILE ITEM DATA                          
*                                                                               
*                                  ** ATTRIBUTES ITEM **                        
TMPIATTR DS    X                   IMPRESSIONS ATTRIBUTE                        
TMPUATTR DS    X                   UNIVERSE ATTRIBUTE                           
TMPCATTR DS    X                   CUMES ATTRIBUTE                              
TMPALEN  EQU   *-TMPAREA                                                        
         ORG   TMPDATA             ** STATION ITEM **                           
TMPSTA   DS    CL5                 STATION                                      
TMPSLEN  EQU   *-TMPAREA                                                        
         ORG   TMPDATA             ** DAYPART ITEM **                           
TMPDDAY  DS    X                   DAYS                                         
TMPDDPT  DS    X                   DAYPART                                      
TMPDMTYP DS    X                   MARKET TYPE                                  
TMPDSVIO DS    C                   STATION VIOLATION                            
TMPDLEN  EQU   *-TMPAREA                                                        
         ORG   TMPDATA             ** HOURS ITEM **                             
TMPHDAY  DS    X                   DAYS                                         
TMPHSQH  DS    X                   START QUARTER HOUR                           
TMPHEQH  DS    X                   END QUARTER HOUR                             
TMPHMTYP DS    X                   MARKET TYPE                                  
TMPHSVIO DS    C                   STATION VIOLATION                            
TMPHLEN  EQU   *-TMPAREA                                                        
         ORG   TMPDATA             ** DEMO VALUES ITEM **                       
TMPVATTR DS    X                   DEMO VALUES ATTRIBUTE                        
*                                      X'40' = MULTIPLY VALUES BY 10            
*                                      LOW NIBBLE IS BYTES/VALUE                
TMPVALS  DS    0X                  DEMO VALUES                                  
TMPVLEN  EQU   *-TMPAREA                                                        
         ORG   TMPAREA+L'TMPAREA   ORG PAST TMPAREA                             
***********************************************************************         
*        MISCELLANEOUS VARIABLES                                                
***********************************************************************         
CURSTA   DS    CL5                 CURRENT STATION                              
NUMOBJS  DS    F                   NUMBER OF DEMO OBJECTS PUT                   
UNITRANS DS    C                   UNIVERSE DATA TRANSFERED (Y/N)               
TMPTWA   DS    F                   TWA NUMBER FOR END OF TEMP FILE              
TMPDNEXT DS    F                   DISP WITHIN LAST TWA FOR END OF TMP          
DUPTAB   DS    1000X               DUPLICATE DAY/DAYPARTS TABLE                 
DCH0     DS    XL1                 X'FF'= DIE                                   
BK5E     DS    XL2                 BOOK ON 5E ELEM OF RECD BEING PROCD          
NEWDEMS  DS    XL64                16 4BYTE DEMOS                               
OLDDEMS  DS    XL84                21 4BYTE DEMOS                               
         EJECT                                                                  
* DEDBLOCK                                                                      
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053CTMAD04A  05/01/02'                                      
         END                                                                    
